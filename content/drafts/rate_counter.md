+++
title = "Rate counter"
description = ""
date = 2019-04-20T15:18:34Z
aliases = []
[extra]
id = 5179
[taxonomies]
categories = []
tags = []
+++

{{task}}Counting the frequency at which something occurs is a common activity in measuring performance and managing resources. In this task, we assume that there is some job which we want to perform repeatedly, and we want to know how quickly these jobs are being performed.

Of interest is the code that performs the actual measurements. Any other code (such as job implementation or dispatching) that is required to demonstrate the rate tracking is helpful, but not the focus.

Multiple approaches are allowed (even preferable), so long as they can accomplish these goals:

* Run N seconds worth of jobs and/or Y jobs.
* Report at least three distinct times.


Be aware of the precision and accuracy limitations of your timing mechanisms, and document them if you can.

'''See also:''' [[System time]], [[Time a function]]



## Ada

Launch 6 jobs in parallel and record the elapsed time for each job. A variant
to get CPU times would use the package Ada.Execution_Time (Ada05).

The precision of measure is given by the value of System.Tick; on Windows value is 10 ms.

```Ada
with System;                     use System;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Unchecked_Deallocation; use Ada;
with Interfaces;

procedure Rate_Counter is
   pragma Priority (Max_Priority);

   package Duration_IO is new Fixed_IO (Duration);

   Job_Nbr : constant := 6; -- adjust to your need
   subtype Job_Index is Natural range 1 .. Job_Nbr;

   task type Job (ID : Job_Index) is
      pragma Priority (Default_Priority);
      entry Start;
   end Job;

   type Job_Ptr is access Job;
   procedure Free is new Unchecked_Deallocation (Job, Job_Ptr);

   Jobs : array (Job_Index) of Job_Ptr;

   Done      : Natural                      := 0;
   Completed : array (Job_Index) of Boolean := (others => False);

   type Timings is array (Job_Index) of Calendar.Time;
   Start_T, Stop_T : Timings;

   task body Job is
      Anchor : Interfaces.Integer_32;
      pragma Volatile (Anchor); -- necessary to avoid compiler optimization.
   begin
      accept Start;

      for I in Interfaces.Integer_32'Range loop      -- the job to do
         Anchor := I;
      end loop;
   end Job;

begin
   for J in Job_Index'Range loop
      Jobs (J) := new Job (ID => J); -- create the jobs first, sync later
   end loop;
   for J in Job_Index'Range loop -- launch the jobs in parallel
      Start_T (J) := Calendar.Clock; -- get the start time
      Jobs (J).Start; -- priority settings necessary to regain control.
   end loop;
   -- Polling for the results / also possible to use a protected type.
   while not (Done = Job_Nbr) loop
      for J in Job_Index'Range loop
         if not Completed (J) and then Jobs (J)'Terminated then
            Stop_T (J) := Calendar.Clock; -- get the end time
            Put ("Job #" & Job_Index'Image (J) & " is finished. It took ");
            Duration_IO.Put (Stop_T (J) - Start_T (J), Fore => 3, Aft => 2);
            Put_Line (" seconds.");
            Completed (J) := True;
            Done          := Done + 1;
         end if;
      end loop;
      delay System.Tick; -- according to the precision of the system clock
   end loop;
   Duration_IO.Put (System.Tick, Fore => 1, Aft => 6);
   Put_Line (" seconds is the precision of System clock.");

   for J in Job_Index'Range loop
      Free (Jobs (J)); -- no GC in Ada, clean-up is explicit
   end loop;

end Rate_Counter;
```


Output on a Linux 64 bits system:
<pre style="overflow: auto; height: 5em;">
Job # 1 is finished. It took  57.93 seconds.
Job # 5 is finished. It took  58.27 seconds.
Job # 6 is finished. It took  58.27 seconds.
Job # 4 is finished. It took  60.42 seconds.
Job # 3 is finished. It took  60.98 seconds.
Job # 2 is finished. It took  61.12 seconds.
0.000001 seconds is the precision of System clock.

```



## AutoHotkey


### Built in variable

The built in variable [http://ahkscript.org/docs/Variables.htm#TickCount A_TickCount] contains the number of milliseconds since the computer was rebooted. Storing this variable and later comparing it to the current value will measure the time elapsed. A_TickCount has a precision of approximately 10ms.

```AutoHotkey
SetBatchLines, -1
Tick := A_TickCount    ; store tickcount
Loop, 1000000 {
    Random, x, 1, 1000000
    Random, y, 1, 1000000
    gcd(x, y)
}
t := A_TickCount - Tick    ; store ticks elapsed
MsgBox, % t / 1000 " Seconds elapsed.`n" Round(1 / (t / 1000000000), 0) " Loop iterations per second."

gcd(a, b) {    ; Euclidean GCD
    while b
        t := b, b := Mod(a, b), a := t
    return, a
}
```

'''Output:'''

```txt
4.250000 Seconds elapsed.
235294 Loop iterations per second.
```



### Query Performance Counter

The [http://www.autohotkey.com/board/topic/48063-qpx-delay-based-on-queryperformancecounter/ QPX function] by SKAN wraps the [http://msdn.microsoft.com/en-us/library/windows/desktop/ms644904%28v=vs.85%29.aspx QueryPerformanceCounter] DLL, and is precise to one thousandth of a millisecond.

```AutoHotkey
SetBatchLines, -1
QPX(1)  ; start timer
Loop, 1000000 {
    Random, x, 1, 1000000
    Random, y, 1, 1000000
    gcd(x, y)
}
t := QPX(0) ; end timer
MsgBox, % t " Seconds elapsed.`n" Round(1 / (t / 1000000), 0) " Loop iterations per second."

QPX( N=0 ) { ; Wrapper for QueryPerformanceCounter()by SKAN | CD: 06/Dec/2009
    Static F,A,Q,P,X ; www.autohotkey.com/forum/viewtopic.php?t=52083 | LM: 10/Dec/2009
    If  ( N && !P )
        Return  DllCall("QueryPerformanceFrequency",Int64P,F) + (X:=A:=0) + DllCall("QueryPerformanceCounter",Int64P,P)
    DllCall("QueryPerformanceCounter",Int64P,Q), A:=A+Q-P, P:=Q, X:=X+1
    Return  ( N && X=N ) ? (X:=X-1)<<64 : ( N=0 && (R:=A/X/F) ) ? ( R + (A:=P:=X:=0) ) : 1
}

gcd(a, b) {    ; Euclidean GCD
    while b
        t := b, b := Mod(a, b), a := t
    return, a
}
```

'''Output:'''

```txt
4.428430 Seconds elapsed.
225814 Loop iterations per second.
```



## BaCon

The TIMER builtin returns the elapsed time since start of program run, in milliseconds.


```freebasic
' Rate counter
FOR i = 1 TO 3
    GOSUB timeit
NEXT

i = 2000
GOSUB timeit
END

LABEL timeit
    iter = 0
    starter = TIMER
    WHILE TRUE DO
        INCR iter
        IF TIMER >= starter + i THEN BREAK
    WEND
    PRINT iter, " iterations in ", i, " millisecond", IIF$(i > 1, "s", "") 
    RETURN
```


{{out}}

```txt
prompt$ ./rate-counter
6169 iterations in 1 millisecond
16025 iterations in 2 milliseconds
23977 iterations in 3 milliseconds
28167202 iterations in 2000 milliseconds
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT "Method 1: Calculate reciprocal of elapsed time:"
      FOR trial% = 1 TO 3
        start% = TIME
        PROCtasktomeasure
        finish% = TIME
        PRINT "Rate = "; 100 / (finish%-start%) " per second"
      NEXT trial%
      
      PRINT '"Method 2: Count completed tasks in one second:"
      FOR trial% = 1 TO 3
        runs% = 0
        finish% = TIME + 100
        REPEAT
          PROCtasktomeasure
          IF TIME < finish% runs% += 1
        UNTIL TIME >= finish%
        PRINT "Rate = "; runs% " per second"
      NEXT trial%
      END
      
      REM This is an example, replace with the task you want to measure
      DEF PROCtasktomeasure
      LOCAL i%
      FOR i% = 1 TO 1000000
      NEXT
      ENDPROC
```

'''Sample output:'''

```txt

Method 1: Calculate reciprocal of elapsed time:
Rate = 9.09090909 per second
Rate = 9.09090909 per second
Rate = 9.09090909 per second

Method 2: Count completed tasks in one second:
Rate = 9 per second
Rate = 9 per second
Rate = 9 per second

```



## C

This code stores all of the data of the rate counter and its configuration in an instance of a struct named '''rate_state_s''', and a function named '''tic_rate''' is called on that struct instance every time we complete a job.  If a configured time has elapsed, '''tic_rate''' calculates and reports the tic rate, and resets the counter.


```c>#include <stdio.h

#include <time.h>

// We only get one-second precision on most systems, as
// time_t only holds seconds.
struct rate_state_s
{
    time_t lastFlush;
    time_t period;
    size_t tickCount;
};

void tic_rate(struct rate_state_s* pRate)
{
    pRate->tickCount += 1;

    time_t now = time(NULL);

    if((now - pRate->lastFlush) >= pRate->period)
    {
        //TPS Report
        size_t tps = 0.0;
        if(pRate->tickCount > 0)
            tps = pRate->tickCount / (now - pRate->lastFlush);

        printf("%u tics per second.\n", tps);

        //Reset
        pRate->tickCount = 0;
        pRate->lastFlush = now;
    }
}

// A stub function that simply represents whatever it is
// that we want to multiple times.
void something_we_do()
{
    // We use volatile here, as many compilers will optimize away
    // the for() loop otherwise, even without optimizations
    // explicitly enabled.
    //
    // volatile tells the compiler not to make any assumptions
    // about the variable, implying that the programmer knows more
    // about that variable than the compiler, in this case.
    volatile size_t anchor = 0;
    size_t x = 0;
    for(x = 0; x < 0xffff; ++x)
    {
        anchor = x;
    }
}

int main()
{
    time_t start = time(NULL);

    struct rate_state_s rateWatch;
    rateWatch.lastFlush = start;
    rateWatch.tickCount = 0;
    rateWatch.period = 5; // Report every five seconds.

    time_t latest = start;
    // Loop for twenty seconds
    for(latest = start; (latest - start) < 20; latest = time(NULL))
    {
        // Do something.
        something_we_do();

        // Note that we did something.
        tic_rate(&rateWatch);
    }

    return 0;
}
```



## C++

This code defines the counter as a class, '''CRateState'''. The counter's period is configured as an argument to its constructor, and the rest of the counter state is kept as class members. A member function '''Tick()''' manages updating the counter state, and reports the tic rate if the configured period has elapsed.


```cpp>#include <iostream

#include <ctime>

// We only get one-second precision on most systems, as
// time_t only holds seconds.
class CRateState
{
protected:
    time_t m_lastFlush;
    time_t m_period;
    size_t m_tickCount;
public:
    CRateState(time_t period);
    void Tick();
};

CRateState::CRateState(time_t period) : m_lastFlush(std::time(NULL)),
                                        m_period(period),
                                        m_tickCount(0)
{ }

void CRateState::Tick()
{
    m_tickCount++;

    time_t now = std::time(NULL);

    if((now - m_lastFlush) >= m_period)
    {
        //TPS Report
        size_t tps = 0.0;
        if(m_tickCount > 0)
            tps = m_tickCount / (now - m_lastFlush);

        std::cout << tps << " tics per second" << std::endl;

        //Reset
        m_tickCount = 0;
        m_lastFlush = now;
    }
}

// A stub function that simply represents whatever it is
// that we want to multiple times.
void something_we_do()
{
    // We use volatile here, as many compilers will optimize away
    // the for() loop otherwise, even without optimizations
    // explicitly enabled.
    //
    // volatile tells the compiler not to make any assumptions
    // about the variable, implying that the programmer knows more
    // about that variable than the compiler, in this case.
    volatile size_t anchor = 0;
    for(size_t x = 0; x < 0xffff; ++x)
    {
        anchor = x;
    }
}

int main()
{
    time_t start = std::time(NULL);

    CRateState rateWatch(5);

    // Loop for twenty seconds
    for(time_t latest = start; (latest - start) < 20; latest = std::time(NULL))
    {
        // Do something.
        something_we_do();

        // Note that we did something.
        rateWatch.Tick();
    }

    return 0;
}
```



## Common Lisp

Common Lisp already has a <code>time</code> macro.

```lisp
(time (do some stuff))
```
 will give a timing report about "stuff" on the trace output.  We can define something similar with repeats:

```lisp
(defmacro time-this (cnt &rest body)
  (let ((real-t (gensym)) (run-t (gensym)))
    `(let (,real-t ,run-t)
       (setf ,real-t (get-internal-real-time)
	     ,run-t  (get-internal-run-time))
       (loop repeat ,cnt do ,@body)
       (list (/ (- (get-internal-real-time) ,real-t)
		(coerce internal-time-units-per-second 'float))
	     (/ (- (get-internal-run-time) ,run-t)
		(coerce internal-time-units-per-second 'float))))))
```


Call the <code>time-this</code> macro to excute a loop 99 times:

```lisp
(print (time-this 99 (loop for i below 10000 sum i)))
```
which gives a pair of numbers, the real time and the run time, both in seconds:<lang>(0.023 0.022)
```



## D



```d

import std.stdio;
import std.conv;
import std.datetime.stopwatch;

int a;
void f0() {}
void f1() { auto b = a; }
void f2() { auto b = to!string(a); }

	
void main()
{
  auto r = benchmark!(f0, f1, f2)(10_000);

  writeln("Time fx took to run 10,000 times:\n");
  writeln("f0: ", r[0]);
  writeln("f1: ", r[1]);
  writeln("f2: ", r[2]);
	
}


```


{{out}}

```txt

Time fx took to run 10,000 times:

f0: 37 μs and 7 hnsecs
f1: 56 μs and 2 hnsecs
f2: 1 ms, 966 μs, and 6 hnsecs


```




## E


```e>def makeLamportSlot := <import:org.erights.e.elib.slot.makeLamportSlot


The rate counter:

/** Returns a function to call to report the event being counted, and an
    EverReporter slot containing the current rate, as a float64 in units of
    events per millisecond. */
def makeRateCounter(timer, reportPeriod) {  
    var count := 0
    var start := timer.now()
    def &rate := makeLamportSlot(nullOk[float64], null)
  
    def signal() {
        def time := timer.now()
        count += 1
        if (time >= start + reportPeriod) {
            rate := count / (time - start)
            start := time
            count := 0
        }
    }
  
    return [signal, &rate]
}
```


The test code:


```e
/** Dummy task: Retrieve http://localhost/ and return the content. */
def theJob() {
    return when (def text := <http://localhost/> <- getText()) -> {
        text
    }
}

/** Repeatedly run 'action' and wait for it until five seconds have elapsed. */
def repeatForFiveSeconds(action) {
    def stopTime := timer.now() + 5000
    def loop() {
        if (timer.now() < stopTime) {
            when (action <- ()) -> {
                loop()
            }
        }
    }
    loop()
}

def whenever := <import:org.erights.e.elib.slot.whenever>

def [signal, &rate] := makeRateCounter(timer, 1000)

# Prepare to report the rate info.
whenever([&rate], fn {
    println(`Rate: ${rate*1000} requests/sec`)
}, fn {true})

# Do some stuff to be counted.
repeatForFiveSeconds(fn {
    signal()
    theJob()
})
```



## Erlang

Measuring elapsed time is built into the timer module. Doing something during a time period requires code. For normal use the Fun should take a large amount of microseconds, our unit of measurement.

```Erlang

-module( rate_counter ).

-export( [fun_during_seconds/2, task/0] ).

fun_during_seconds( Fun, Seconds ) ->
	My_pid = erlang:self(),
	Ref = erlang:make_ref(),
        Pid = erlang:spawn( fun() -> fun_during_seconds_loop( My_pid, Fun ) end ),
        timer:send_after( Seconds * 1000, My_pid, {stop, Ref} ),
	N = fun_during_seconds_receive_loop( Ref, Pid, 0 ),
	erlang:exit( Pid, kill ),
	N.

task() ->
    Results = [timer:tc( fun() -> io:fwrite("Hello, world!~n") end ) || _X <- lists:seq(1, 3)],
    Times = [X || {X, _Returned} <- Results],
    io:fwrite( "Times ~p, average ~p microseconds.~n", [Times, lists:sum(Times) / erlang:length(Times)]),
    N =	fun_during_seconds( fun() -> math:sqrt(123) end, 2 ),
    io:fwrite( "Square root of 123, during 2	seconds, was done ~p times.~n", [N] ).



fun_during_seconds_loop( Pid, Fun ) ->
	Fun(),
	Pid ! {one_time, erlang:self()},
	fun_during_seconds_loop( Pid, Fun ).

fun_during_seconds_receive_loop( Ref, Pid, N ) ->
	receive
	{stop, Ref} -> N;
        {one_time, Pid} -> fun_during_seconds_receive_loop( Ref, Pid, N + 1 )
	end.


```

{{out}}

```txt

19> rate_counter:task().
Hello, world!
Hello, world!
Hello, world!
Times [54,26,52], average 44.0 microseconds.
Square root of 123, during 2 seconds, was done 6398906 times.

```



## ERRE


```ERRE

PROGRAM RATE_COUNTER

!
! for rosettacode.org
!

!
! This is an example, replace with the task you want to  measure
!
PROCEDURE TASK_TO_MEASURE
  LOCAL I
    FOR I=1 TO 1000000 DO
    END FOR
END PROCEDURE

BEGIN
    PRINT("Method 1: Calculate reciprocal of elapsed time:")
    FOR TRIAL%=1 TO 3 DO
      START=TIMER
      TASK_TO_MEASURE
      FINISH=TIMER
      PRINT("Rate =";100/(FINISH-START);"per second")
    END FOR

    PRINT("Method 2: Count completed tasks in one minute:")
    FOR TRIAL%=1 TO 3 DO
      RUNS%=0
      FINISH=TIMER+60
      REPEAT
        TASK_TO_MEASURE
        IF TIMER<FINISH THEN RUNS%+=1 END IF
      UNTIL TIMER>=FINISH
      PRINT("Rate =";RUNS%;"per minute")
    END FOR
END PROGRAM

```

Time elapsed is measured with TIMER function (taken from computer clock).
{{out}}

```txt

Method 1: Calculate reciprocal of elapsed time:
Rate = 25.24655 per second
Rate = 25.32147 per second
Rate = 25.6513 per second
Method 2: Count completed tasks in one minute:
Rate = 15 per second
Rate = 15 per second
Rate = 15 per second

```



## Fortran

Standard Fortran does not offer facilities for starting another task, nor for monitoring such a task's consumption of cpu time against clock time. However, a program can monitor its ''own'' usage by invoking a suitable routine at appropriate points in its computation, say on each new iteration of its outermost DO-loop, and thus generate progress reports that could also include an estimated time of finishing. This requires access to system timers, usually achieved via invocations of special routines that are often specific to an installation. But F90 introduced the intrinsic <code>CALL CPU_TIME(T)</code> that returns a "processor-dependent approximation of the processor time in seconds" in <code>T</code> a floating-point variable.

Similarly, an installation may offer local routines to report the date and time, and F90 has introduced an intrinsic that can be invoked as <code>CALL DATE_AND_TIME(VALUES = MARK)</code> where MARK is an eight-element integer array, rather exhaustingly returning year, month, day, minutes from GMT (or UT, ''etc''), hour, minute, second, milliseconds. 

So, in 
```Fortran
      DO I = FIRST,LAST
        IF (PROGRESSNOTE((I - FIRST)/(LAST - FIRST + 1.0))) WRITE (6,*) "Reached ",I,", towards ",LAST
        ...much computation...
      END DO
```

Function PROGRESSNOTE is invoked at the start of each iteration, with its parameter stating how much progress has been made on a scale of zero to one, with a "zero progress" restarting its timers. The function notes whether sufficient clock time has elapsed since its previous report (more than six seconds, for example) and if so, returns ''true'' after starting an output line with a standard report giving an estimated time to run and an estimated time (and date, if not the current day) of finishing. This line is not terminated; the invoking routine appends its own progress message, tailored to the nature of the task it is working through. For instance,

```txt

                              Standard progress report|Tailored message.
ETF + 6·2hrs!@Monday    17/ 7/2017  5:23:25·013am.  0% Dumping Monday     3/ 2/1749.
ETF + 6·2hrs!@Monday    17/ 7/2017  5:23:37·167am.  0% Dumping Sunday     9/ 3/1749.
ETF + 6·2hrs!@Monday    17/ 7/2017  5:26:06·383am.  0% Dumping Friday    11/ 4/1749.
ETF + 6·1hrs!@Monday    17/ 7/2017  5:21:23·397am.  0% Dumping Friday    16/ 5/1749.

```

Thus, the human waiting at the computer screen can monitor the rate of progress and know to go for a walk, or not.

Incidentally, on windows systems at least, frequent invocations of the date and time routine can cause execution to run ''much'' slower, or worse. A loop waiting for the system's DATE_AND_TIME result to attain a specified value will instead cause a crash.

For another approach, imagine a long-running program, WORKER, that writes various remarks to standard output as it goes, and consider another, TIMESTAMP, that copies from standard input to standard output, prefixing each line with a date and time stamp, perhaps invoked via something like <code>WORKER | TIMESTAMP >Log.txt</code> - the vertical bar an amusing choice to symbolise a horizontal "pipe". When everything finishes, the log file can be analysed to determine the rate of progress. But alas, in the windows world, the stages of a "pipeline" are performed serially, not simultaneously - the vertical bar symbolising this separation. All output from WORKER will be saved in a temporary disc file then when WORKER finishes that file will be fed as input to TIMESTAMP, thereby producing data only on the rate of file input/output.


## Go

{{trans|C}}

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// representation of time.Time is nanosecond, actual resolution system specific
type rateStateS struct {
    lastFlush time.Time
    period    time.Duration
    tickCount int
}

func ticRate(pRate *rateStateS) {
    pRate.tickCount++
    now := time.Now()
    if now.Sub(pRate.lastFlush) >= pRate.period {
        // TPS Report
        tps := 0.
        if pRate.tickCount > 0 {
            tps = float64(pRate.tickCount) / now.Sub(pRate.lastFlush).Seconds()
        }
        fmt.Println(tps, "tics per second.")

        // Reset
        pRate.tickCount = 0
        pRate.lastFlush = now
    }
}

func somethingWeDo() {
    time.Sleep(time.Duration(9e7 + rand.Int63n(2e7))) // sleep about .1 second.
}

func main() {
    start := time.Now()

    rateWatch := rateStateS{
        lastFlush: start,
        period:    5 * time.Second,
    }

    // Loop for twenty seconds
    latest := start
    for latest.Sub(start) < 20*time.Second {
        somethingWeDo()
        ticRate(&rateWatch)
        latest = time.Now()
    }
}
```

Output:

```txt

9.941784884430728 tics per second.
10.01399996465647 tics per second.
9.848572291869138 tics per second.

```



## Haskell

This solution returns the time deltas in picosecond resolution.

```haskell

import Control.Monad
import Control.Concurrent
import Data.Time

getTime :: IO DiffTime
getTime = fmap utctDayTime getCurrentTime

addSample :: MVar [a] -> a -> IO ()
addSample q v = modifyMVar_ q (return . (v:))

timeit :: Int -> IO a -> IO [DiffTime]
timeit n task = do
    samples <- newMVar []
    forM_ [0..n] $ \n -> do
        t1 <- getTime
        task
        t2 <- getTime
        addSample samples (t2 - t1)

    readMVar samples

main = timeit 10 (threadDelay 1000000)

```



## HicEst

The script opens a modeless dialog with 3 buttons: "Hits++" to increase Hits, "Count 5 sec" to reset Hits and initialize a delayed call to F5 after 5 sec, "Rate" to display the current rate on the status bar.

```HicEst
CHARACTER prompt='Count "Hits++" for 5 sec, get current rate'

DLG(Button="1:&Hits++", CALL="cb", B="2:&Count 5sec", B="3:&Rate", RC=retcod, TItle=prompt, WIN=hdl)

SUBROUTINE cb              ! callback after dialog buttons
  IF(retcod == 1) THEN     ! "Hits++" button
    Hits = Hits + 1
  ELSEIF(retcod == 2) THEN ! "Count 5 sec" button
    Hits = 0
    ALARM(5, 5)            ! call F5 in 5 seconds
    t_start = TIME()
  ELSE                     ! "Rate" button
    sec = TIME() - t_start
    WRITE(StatusBar) 'Average rate since last "5 sec" button = ', hits/sec, " Hz"
  ENDIF
END

SUBROUTINE F5 ! called 5 sec after button "5 sec"
  WRITE(StatusBar) Hits, "hits last 5 sec"
END
```



## J

'''Solution'''


```j
   x (6!:2) y
```

The foreign conjunction <code>6!:2</code> will execute the code <code>y</code> (right argument), <code>x</code> times (left argument) and report the average time in seconds required for one execution.

'''Example:'''

```j
   list=: 1e6 ?@$ 100           NB. 1 million random integers from 0 to 99
   freqtable=: ~. ,. #/.~       NB. verb to calculate and build frequency table
   20 (6!:2) 'freqtable list'   NB. calculate and build frequency table for list, 20 times
0.00994106
```


Note, if instead we want distinct times instead of averaged times we can use a repeated counter for the number of times to execute the code


```j
   1 1 1 (6!:2) 'freqtable list'
0.0509995 0.0116702 0.0116266
```



## Java

{{trans|JavaScript}}
{{works with|Java|8}}

```java
import java.util.function.Consumer;

public class RateCounter {

    public static void main(String[] args) {
        for (double d : benchmark(10, x -> System.out.print(""), 10))
            System.out.println(d);
    }

    static double[] benchmark(int n, Consumer<Integer> f, int arg) {
        double[] timings = new double[n];
        for (int i = 0; i < n; i++) {
            long time = System.nanoTime();
            f.accept(arg);
            timings[i] = System.nanoTime() - time;
        }
        return timings;
    }
}
```



```txt
70469.0
2047.0
1169.0
877.0
877.0
877.0
877.0
877.0
877.0
877.0
```



###  Stream based solution 

{{trans|JavaScript}}
{{works with|Java|8}}

```java
import java.util.function.IntConsumer;
import java.util.stream.DoubleStream;

import static java.lang.System.nanoTime;
import static java.util.stream.DoubleStream.generate;

import static java.lang.System.out;

public interface RateCounter {
  public static void main(final String... arguments) {
    benchmark(
      10,
      x -> out.print(""),
      10
    )
      .forEach(out::println)
    ;
  }

  public static DoubleStream benchmark(
    final int n,
    final IntConsumer consumer,
    final int argument
  ) {
    return generate(() -> {
      final long time = nanoTime();
      consumer.accept(argument);
      return nanoTime() - time;
    })
      .limit(n)
    ;
  }
}
```



```txt
81431.0
3987.0
3205.0
3081.0
3020.0
3101.0
3040.0
3102.0
3072.0
3060.0
```



## JavaScript

The ''benchmark'' function below executes a given function n times, calling it with the specified arguments. After execution of all functions, it returns an array with the execution time of each execution, in milliseconds.


```javascript
function millis() { // Gets current time in milliseconds.
  return (new Date()).getTime();
}

/* Executes function 'func' n times, returns array of execution times. */
function benchmark(n, func, args) {
  var times = [];
  for (var i=0; i<n; i++) {
    var m = millis();
    func.apply(func, args);
    times.push(millis() - m);
  }
  return times;
}
```



## Jsish


```javascript
#!/usr/bin/env jsish
"use strict";
/* Rate counter, timer access, in Jsish */

/* System time in milliseconds */
var runs = 0, newMs;
function countJobsIsTheJob() { runs += 1; }
var milliSeconds = strptime();
while ((newMs = strptime()) < (milliSeconds + 1000)) { countJobsIsTheJob(); }
puts(runs, 'runs in', newMs - milliSeconds, 'ms');


/* Builtin times test(callback, runs), result in microseconds */
function sleeper() { sleep(10); }

var timer;
for (var i = 1; i < 4; i++) {
    timer = times(sleeper, 100);
    puts(timer, 'μs to sleep 10 ms, 100 times');
}
```


{{out}}

```txt

prompt$ jsish rateCounter.jsi
81494 runs in 1000 ms
1019410 μs to sleep 10 ms, 100 times
1018384 μs to sleep 10 ms, 100 times
1018984 μs to sleep 10 ms, 100 times
```



## Julia

The elapsed() macro in Julia generally is accurate in the nanosecond range.

```julia
dosomething() = sleep(abs(randn()))

function runNsecondsworthofjobs(N)
    times = Vector{Float64}()
    totaltime = 0
    runcount = 0
    while totaltime < N
        t = @elapsed(dosomething())
        push!(times, t)
        totaltime += t
        runcount += 1
    end
    println("Ran job $runcount times, for total time of $totaltime seconds.")
    println("Average time per run was $(sum(times)/length(times)) seconds.")
    println("Individual times of the jobs in seconds were:")
    for t in times
        println("    $t")
    end
end

runNsecondsworthofjobs(5)

```
{{output}}
```txt
 Ran job 5 times, for total time of 5.215301074 seconds.
 Average time per run was 1.0430602148 seconds.
 Individual times of the jobs in seconds were:
     1.901202753
     0.706044625
     0.485377196
     0.489283165
     1.633393335

```



## Kotlin

{{trans|JavaScript}}

```scala
// version 1.1.3

typealias Func<T> = (T) -> T

fun cube(n: Int) = n * n * n

fun <T> benchmark(n: Int, func: Func<T>, arg: T): LongArray {
    val times = LongArray(n)
    for (i in 0 until n) {
         val m = System.nanoTime()
         func(arg)
         times[i] = System.nanoTime() - m
    }
    return times
} 

fun main(args: Array<String>) {
    println("\nTimings (nanoseconds) : ")
    for (time in benchmark(10, ::cube, 5)) println(time)
}
```


Sample output:

```txt

154430
2100
1275
1138
1063
1113
1087
1088
1063
1025

```



## Liberty BASIC

precision depends on OS. It is 16 (sometines cames as 15) ms for XP and 10 ms for Win2000.

```lb

Print "Rate counter"
print "Precision: system clock, ms ";
t0=time$("ms")
while time$("ms")=t0    'busy loop till click ticks
wend
print time$("ms")-t0
print

Print "Run jobs N times, report every time"
Print "After that, report average time"
N=10
t00=time$("ms")
for i = 1 to 10
    scan
    t0=time$("ms")
    'any code we want to measure goes here
    res = testFunc()
    'end of measured code
    t1=time$("ms")
    ElapsedTime = t1-t0
    print "Job #";i;" Elapsed time, ms ";ElapsedTime, 1000/ElapsedTime; " ticks per second"
next
print "---------------------------------"
print "Average time, ms, is ";(t1-t00)/N,  1000/((t1-t00)/N); " ticks per second"


print
print "Run jobs for not less then N seconds (if time up, it'll finish last job)"
print "After that, report average time"

NSec=5
i = 0
t00=time$("ms")
while time$("ms")<t00+NSec*1000
    scan
    i = i+1
    t0=time$("ms")
    'any code we want to measure goes here
    res = testFunc()
    'end of measured code
    t1=time$("ms")
    ElapsedTime = t1-t0
    print "Job #";i;" Elapsed time, ms ";ElapsedTime,  1000/ElapsedTime; " ticks per second"
wend
print "---------------------------------"
print "Average time, ms, is ";(t1-t00)/i,  1000/((t1-t00)/i); " ticks per second"

end

function testFunc()
    s=0
    for i = 1 to 30000
        s=s+sin(i)/30000
    next
    testFunc = s
end function 

```



## OxygenBasic

Rate Counter Deluxe, giving start and finish times + duration. The duration is measured in seconds using the system performance counter, resolved to the nearest microsecond.

```oxygenbasic

'
### ==

'TIME API
'
### ==


'http://msdn.microsoft.com/en-us/library/windows/desktop/ms724950(v=vs.85).aspx

extern lib "kernel32.dll"

type SYSTEMTIME
  WORD wYear
  WORD wMonth
  WORD wDayOfWeek
  WORD wDay
  WORD wHour
  WORD wMinute
  WORD wSecond
  WORD wMilliseconds
end type

void GetSystemTime(SYSTEMTIME*t)
void GetLocalTime(SYSTEMTIME*t)
void QueryPerformanceCounter(quad*c)
void QueryPerformanceFrequency(quad*freq)
void Sleep(sys millisecods)

end extern

String WeekDay[7]={"Sunday","Monday","Tuesday","Wednesday",
"Thursday","Friday","Saturday"}

String MonthName[12]={"January","February","March","April","May","June",
"July","August","September","October","November","December"}


'
### ========

Class Jobrecord
'
### ========

  
  has SYSTEMTIME stt
  has SYSTEMTIME fin
  quad countA
  quad CountB
  quad freq
  sys  serial

  method pad(string s) as string
    method=s
    if len(method)<2 then method="0"+method
  end method


  method ShowDateTime(sys a,f) as string

  SYSTEMTIME *t

  if a then
    @t=@fin
  else
    @t=@stt
  end if
  '
  String month=pad(str t.wMonth)
  String day=pad(str t.wDay)
  if f=0 then 
    return "" t.wYear "-" month "-" day "    "+
    pad(t.wHour) ":" pad(t.wMinute) ":" pad(t.wSecond) ":" t.wMilliSeconds
  elseif f=1
    return WeekDay[t.wDayOfWeek+1 and 7 ] " " +
    MonthName[t.wMonth and 31] " " day " " t.wYear
  end if
  end method

  method Start()
  QueryPerformanceCounter countA
  QueryPerformanceFrequency freq
  serial++
  GetLocalTime stt
  end method

  method Finish()
  GetLocalTime fin
  QueryPerformanceCounter countB
  end method


  method ShowDuration() as string
  return str((countB-countA)/freq,6) 'seconds with microsecond resolution
  end method

  method report() as string
  string tab=chr(9), cr=chr(13)+chr(10)
  method="Job:" tab serial cr +
  "Duration:"   tab ShowDuration() cr +
  "Start: "     tab ShowDateTime(0,0) cr +
  "Finish:"     tab ShowDateTime(1,0) cr +
  ShowDateTime(1,1) cr
  end method

end class

'#recordof JobRecord

'====
'TEST
'====

JobRecord JR
JR.start
sleep 100 'JOB!
JR.finish
print JR.Report
'putfile "s.txt",JR.Report
'
'Job:	1
'Duration:	0.099026
'Start: 	2012-07-01    00:52:36:874
'Finish:	2012-07-01    00:52:36:974
'Sunday July 01 2012

```



## Mathematica

The first parameter for both of these functions can be any program code.

<lang>jobRateCounted[fn_,Y_Integer]:=First[AbsoluteTiming[Do[fn,{Y}]]/Y;
SetAttributes[jobRateCounted,HoldFirst]

jobRatePeriod[fn_,time_]:=Block[{n=0},TimeConstrained[While[True,fn;n++]];n/time];
SetAttributes[jobRatePeriod,HoldFirst]
```



## PARI/GP


```parigp
a=0;
b=0;
for(n=1,20000000,
  a=a+gettime();
  if(a>60000,print(b);a=0;b=0);
'''code to test'''
  b=b+1;
  a=a+gettime();
  if(a>60000,print(b);a=0;b=0)
)
```



## Perl

The [http://perldoc.perl.org/Benchmark.html Benchmark] module can rate code per time, or per loops executed:

```perl
use Benchmark;

timethese COUNT,{ 'Job1' => &job1, 'Job2' => &job2 };

sub job1
{
	...job1 code...
}
sub job2
{
	...job2 code...
}
```

A negative COUNT will run each job for at least COUNT seconds.

A positive COUNT will run each job COUNT times.

## Perl 6


```perl6
sub runrate($N where $N > 0, &todo) {
    my $n = $N;

    my $start = now;
    todo() while --$n;
    my $end = now;

    say "Start time: ", DateTime.new($start).Str;
    say "End time: ", DateTime.new($end).Str;
    my $elapsed = $end - $start;

    say "Elapsed time: $elapsed seconds";
    say "Rate: { ($N / $elapsed).fmt('%.2f') } per second\n";
}

sub factorial($n) { (state @)[$n] //= $n < 2 ?? 1 !! $n * factorial($n-1) }

runrate 10000, { state $n = 1; factorial($n++) }

runrate 10000, { state $n = 1; factorial($n++) }
```

{{out}}

```txt
Start time: 2013-03-08T20:57:02Z
End time: 2013-03-08T20:57:03Z
Elapsed time: 1.5467497 seconds
Rate: 6465.17 per second

Start time: 2013-03-08T20:57:03Z
End time: 2013-03-08T20:57:04Z
Elapsed time: 0.7036318 seconds
Rate: 14211.98 per second
```

The <tt>Instant</tt> type in Perl 6 is defined to be based on TAI seconds, and represented with rational numbers that are more than sufficiently accurate to represent your clock's accuracy.  The actual accuracy will depend on your clock's accuracy (even if you don't have an atomic clock in your kitchen, your smartphone can track various orbiting atomic clocks, right?) modulo the vagaries of returning the atomic time (or unreasonable facsimile) via system calls and library APIs.


## Phix

On windows, time() advances in ~0.015s increments, whereas on linux it is ~0.0000016s.

```Phix
procedure task_to_measure()
    sleep(0.1)
end procedure
 
printf(1,"method 1: calculate reciprocal of elapsed time:\n")
for trial=1 to 3 do
    atom t=time()
    task_to_measure()
    t = time()-t
    string r = iff(t?sprintf("%g",1/t):"inf")
    printf(1,"rate = %s per second\n",{r})
end for
 
printf(1,"method 2: count completed tasks in one second:\n")
for trial=1 to 3 do
    integer runs=0
    atom finish=time()+1
    while true do
        task_to_measure()
        if time()>=finish then exit end if
        runs += 1
    end while
    printf(1,"rate = %d per second\n",runs)
end for
```

{{out}}
Of course it fails to achieve the perfect 10/s, due to the overhead of call/ret/time/printf etc.

```txt

method 1: calculate reciprocal of elapsed time:
rate = 9.17431 per second
rate = 9.09091 per second
rate = 9.17431 per second
method 2: count completed tasks in one second:
rate = 9 per second
rate = 9 per second
rate = 9 per second

```



## PicoLisp

[http://software-lab.de/doc/refU.html#usec usec] returns a relative time in
microseconds. This can be used, for example, to measure the time between two key
strokes

```PicoLisp
(prin "Hit a key ... ")
(key)
(prinl)
(let Usec (usec)
   (prin "Hit another key ... ")
   (key)
   (prinl)
   (prinl "This took " (format (- (usec) Usec) 6) " seconds") )
```

Output:

```txt
Hit a key ... 
Hit another key ... 
This took 3.132058 seconds
```

The [http://software-lab.de/doc/refB.html#bench bench] benchmark function could
also be used. Here we measure the time until a key is pressed

```PicoLisp
(bench (key))
```


```txt
1.761 sec
-> "a"
```



## PowerShell


```PowerShell

[datetime]$start = Get-Date

[int]$count = 3

[timespan[]]$times = for ($i = 0; $i -lt $count; $i++)
{ 
    Measure-Command {0..999999 | Out-Null}
}

[datetime]$end = Get-Date

$rate = [PSCustomObject]@{
    StartTime      = $start
    EndTime        = $end
    Duration       = ($end - $start).TotalSeconds
    TimesRun       = $count
    AverageRunTime = ($times.TotalSeconds | Measure-Object -Average).Average
}

$rate | Format-List

```

{{Out}}

```txt

StartTime      : 10/27/2016 3:33:16 PM
EndTime        : 10/27/2016 3:33:30 PM
Duration       : 13.9062588
TimesRun       : 3
AverageRunTime : 4.63301593333333

```



## PureBasic


### Counting frequence of an event


```PureBasic
Procedure.d TimesPSec(Reset=#False)
  Static starttime, cnt
  Protected Result.d, dt
  If Reset
    starttime=ElapsedMilliseconds(): cnt=0
  Else
    cnt+1
    dt=(ElapsedMilliseconds()-starttime)
    If dt
      Result=cnt/(ElapsedMilliseconds()-starttime)
    EndIf
  EndIf
  ProcedureReturn Result*1000
EndProcedure

If OpenWindow(0,#PB_Ignore,#PB_Ignore,220,110,"",#PB_Window_SystemMenu)
  Define Event, r.d, GadgetNumber
  ButtonGadget(0,10, 5,200,35,"Click me!")
  ButtonGadget(1,10,70,100,35,"Reset")
  TextGadget  (2,10,45,200,25,"")
  TimesPSec(1)
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Gadget 
      GadgetNumber =EventGadget()
      If GadgetNumber=0
        r=TimesPSec()
        SetGadgetText(2,"You are clicking at "+StrD(r,5)+" Hz.")
      ElseIf GadgetNumber=1
        TimesPSec(1)
        SetGadgetText(2,"Counter zeroed.")
      EndIf
    EndIf
  Until Event=#PB_Event_CloseWindow
EndIf
```



### Counting events for a time period


```PureBasic
Procedure DummyThread(arg)
  Define.d dummy=#PI*Pow(arg,2)/4
EndProcedure

start=ElapsedMilliseconds()
Repeat
  T=CreateThread(@DummyThread(),Random(100))
  WaitThread(T)
  cnt+1
Until start+10000<=ElapsedMilliseconds(); Count for 10 sec

msg$="We got "+Str(cnt)+" st."+Chr(10)+StrF(cnt/10,2)+" threads per sec."
MessageRequester("Counting threads in 10 sec",msg$)
```



## Python


```python
import subprocess
import time

class Tlogger(object):
    def __init__(self):
        self.counts = 0
        self.tottime = 0.0
        self.laststart = 0.0
        self.lastreport = time.time()

    def logstart(self):
        self.laststart = time.time()

    def logend(self):
        self.counts +=1
        self.tottime += (time.time()-self.laststart)
        if (time.time()-self.lastreport)>5.0:   # report once every 5 seconds
           self.report()

    def report(self):
        if ( self.counts > 4*self.tottime):
            print "Subtask execution rate: %f times/second"% (self.counts/self.tottime);
        else:
            print "Average execution time: %f seconds"%(self.tottime/self.counts);
        self.lastreport = time.time()


def taskTimer( n, subproc_args ):
    logger = Tlogger()

    for x in range(n):
        logger.logstart()
        p = subprocess.Popen(subproc_args)
        p.wait()
        logger.logend()
    logger.report()


import timeit
import sys

def main( ):

    # for accurate timing of code segments 
    s = """j = [4*n for n in range(50)]"""
    timer = timeit.Timer(s)
    rzlts = timer.repeat(5, 5000)
    for t in rzlts:
        print "Time for 5000 executions of statement = ",t
    
    # subprocess execution timing
    print "#times:",sys.argv[1]
    print "Command:",sys.argv[2:]
    print ""
    for k in range(3):
       taskTimer( int(sys.argv[1]), sys.argv[2:])

main()
```

Usage Example:
First argument is the number of times to iterate. Additional arguments are command to execute.

```txt
C:>rateCounter.py 20 md5.exe
```



## Racket


```Racket

#lang racket

;; Racket has a useful `time*' macro that does just what's requested:
;; run some expression N times, and produce timing results
(require unstable/time)

;; Sample use:
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(time* 10 (fib 38))

;; But of course, can be used to measure external processes too:
(time* 10 (system "sleep 1"))

```


Sample output:

```txt

; run #1... -> 39088169
; run #2... -> 39088169
; run #3... -> 39088169
; run #4... -> 39088169
; run #5... -> 39088169
; run #6... -> 39088169
; run #7... -> 39088169
; run #8... -> 39088169
; run #9... -> 39088169
; run #10... -> 39088169
; 10 runs, 2 best/worst removed, 6 left for average:
; cpu time: 778ms = 778ms + 0ms gc; real time: 780ms
39088169
; run #1... -> #t
; run #2... -> #t
; run #3... -> #t
; run #4... -> #t
; run #5... -> #t
; run #6... -> #t
; run #7... -> #t
; run #8... -> #t
; run #9... -> #t
; run #10... -> #t
; 10 runs, 2 best/worst removed, 6 left for average:
; cpu time: 3ms = 3ms + 0ms gc; real time: 1007ms
#t

```



## REXX

Programming note:   The   '''$CALC'''   (REXX) program which is invoked below is a general purpose calculator which supports a multitude

of functions (over 1,500),   and can show the results in many different formats   (some of which are shown here).

```rexx
/*REXX program reports on the amount of elapsed time 4 different tasks use (wall clock).*/
time.=                                           /*nullify times for all the tasks below*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
call time 'Reset'                                /*reset the REXX (elapsed) clock timer.*/
                                                 /*show pi in hex to  2,000 dec. digits.*/
                  task.1= 'base(pi,16)  ;;;  lowercase   digits 2k   echoOptions'
                  call '$CALC' task.1            /*perform task number one  (via $CALC).*/
time.1=time('E')                                 /*get and save the time used by task 1.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
call time 'Reset'                                /*reset the REXX (elapsed) clock timer.*/
                                                 /*get primes  40000 ──► 40800 and      */
                                                 /*show their differences.              */
                  task.2= 'diffs[ prime(40k, 40.8k) ]  ;;;  GRoup 20'
                  call '$CALC' task.2            /*perform task number two  (via $CALC).*/
time.2=time('E')                                 /*get and save the time used by task 2.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
call time 'Reset'                                /*reset the REXX (elapsed) clock timer.*/
                                                 /*show the  Collatz sequence  for a    */
                                                 /*stupidly gihugeic number.            */
                  task.3= 'Collatz(38**8)  ;;;  Horizontal'
                  call '$CALC' task.3            /*perform task number three (via $CALC)*/
time.3=time('E')                                 /*get and save the time used by task 3.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
call time 'Reset'                                /*reset the REXX (elapsed) clock timer.*/
                                                 /*plot  SINE  in  ½  degree increments.*/
                                                 /*using five decimal digits  (¬ 60).   */
                  task.4= 'sinD(-180, +180, 0.5)  ;;;  Plot  DIGits 5   echoOptions'
                  call '$CALC' task.4            /*perform task number four (via $CALC).*/
time.4=time('E')                                 /*get and save the time used by task 4.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
say
    do j=1  while  time.j\==''
    say 'time used for task'     j     "was"     right(format(time.j,,0),4)     'seconds.'
    end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   (of the tasks as well as the above REXX timer program):

(The terminal screen size used was '''60''' deep x '''100''' wide.)
<pre style="height:140ex">
                        ╔════════════════════════════════════════════════╗
                        ║ base(pi,16);;; lowercase digits 2k echoOptions ║
                        ╚════════════════════════════════════════════════╝
3.243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89452821e638d01377be5466cf34e90c6cc
0ac29b7c97c50dd3f84d5b5b54709179216d5d98979fb1bd1310ba698dfb5ac2ffd72dbd01adfb7b8e1afed6a267e96ba7c
9045f12c7f9924a19947b3916cf70801f2e2858efc16636920d871574e69a458fea3f4933d7e0d95748f728eb658718bcd5
882154aee7b54a41dc25a59b59c30d5392af26013c5d1b023286085f0ca417918b8db38ef8e79dcb0603a180e6c9e0e8bb0
1e8a3ed71577c1bd314b2778af2fda55605c60e65525f3aa55ab945748986263e8144055ca396a2aab10b6b4cc5c341141e
8cea15486af7c72e993b3ee1411636fbc2a2ba9c55d741831f6ce5c3e169b87931eafd6ba336c24cf5c7a32538128958677
3b8f48986b4bb9afc4bfe81b6628219361d809ccfb21a991487cac605dec8032ef845d5de98575b1dc262302eb651b88238
93e81d396acc50f6d6ff383f442392e0b4482a484200469c8f04a9e1f9b5e21c66842f6e96c9a670c9c61abd388f06a51a0
d2d8542f68960fa728ab5133a36eef0b6c137a3be4ba3bf0507efb2a98a1f1651d39af017666ca593e82430e888cee86194
56f9fb47d84a5c33b8b5ebee06f75d885c12073401a449f56c16aa64ed3aa62363f77061bfedf72429b023d37d0d724d00a
1248db0fead349f1c09b075372c980991b7b25d479d8f6e8def7e3fe501ab6794c3b976ce0bd04c006bac1a94fb6409f60c
45e5c9ec2196a246368fb6faf3e6c53b51339b2eb3b52ec6f6dfc511f9b30952ccc814544af5ebd09bee3d004de334afd66
0f2807192e4bb3c0cba85745c8740fd20b5f39b9d3fbdb5579c0bd1a60320ad6a100c6402c7279679f25fefb1fa3cc8ea5e
9f8db3222f83c7516dffd616b152f501ec8ad0552ab323db5fafd23876053317b483e00df829e5c57bbca6f8ca01a87562e
df1769dbd542a8f6287effc3ac6732c68c4f5573695b27b0bbca58c8e1ffa35db8f011a010fa3d98fd2183b84afcb56c2dd
1d35b9a53e479b6f84565d28e49bc4bfb9790e1ddf2daa4cb7e3362fb1341cee4c6e8ef20cada36774c01d07e9efe2bf11f
b495dbda4dae909198eaad8e716b93d5a0d08ed1d0afc725e08e3c5b2f8e7594b78ff6e2fbf2122b648cb209fda49d89455
e99887a81cf7dc407e83568cdc24fd608c80225f7ada98babf283a8e1b06bbdbb6e99f6b4bc3e795e7be1c57b21085778ab
866f897578cec3600fb01b0789912575fefdc4595bf054658d676f6323cd6db1584bc6747713a2a431395d62de6646642e9
a995fb71811b93af99e6eb7b169c96740aa3a0f9ea3244ab192f10b595dc3e27cfec33f1341a2830a7a30cc356b0a13aa06
a5cffb2b87f9ae0dac27c0f649d4b5f0339
                                  ╔════════════════════════════╗
                                  ║ diffs[ prime(40k, 40.8k) ] ║
                                  ╚════════════════════════════╝
  1► 30 12  2  4 14 42  4  2  4 20  4  2 10  2 10 20 10  6  6 20
 21► 10 14 10  2 34  6 78 12 18 12 12  2  6 18  6  6  4  8 18 10
 41►  8 22  2 10  2 36  4  6  8  4  6  6  8 12 10  6 14  4 60 14
 61► 46  6 18  6 12 12 12 14 16 24 12 14 28 30  8 10  8  4 18  8
 81► 12 10 12  2  6 12 22  8 16  6 14  6  4 12 14 10  8  6  6  4
101► 14  6  4 18  8  4 20 18 48  4  2  4 36 20 10  6  8 22  8 16
121► 14 22 20 12 12 18 18 22  6 12 30 14  6 12 16  6  8 12  4  2
141► 22 30  2 16 18 14  6  6 24  6  4  2 12  6 12  4 26 30 24 34
161► 20  4  8  4  6 12 20 22  6  2 16  6 56 10 14 10 14  4  2 10
181► 20 18 28 14 24  4  8 12 16  6  6  2  6  6 10 14  4 42 18  6
201►  2  4  6  8 12 30 24  4 24  6  6  8 18  4 20  4  2 18  4  6
221►  2 12 12 10  6  8  6 16 14 16  8 10 24  2 10 24  2 18 24  6
241► 10 14 46 14 30 10 26 30 12 24  4 12 30  2 10  8  4  6  8  4
261► 30  8 28  6 14 10 20 10 12  8 10  2 24 10 24 14 10  8  4 20
281► 18 10  6  6 14 34  8 10 14  6 22 26 12 10  8  6 18  6  4  6
301►  6 14 22  2 16  2 10 14 10  6 14 24 22  8 16 18 20 28  8 10
321► 24  6 12 12 20  6  6  6 22  2 18 10 12  8  6 22 14 16 24 18
341►  2 24 12 22  8  4 24 14  6 22  8 10  2 28  2  4 38 12 34 20
361► 10  2  4  8 18  4 48 12 24  6 18 12  6  8 10 42 24 14 60 24
381► 36 12 22  8 12 12  6  4 18 20 12 10  8  6 24  6  4 30  6  2
401► 54 48 36  4 12  8 12  6 22  6  6 14 10 32 18 12 10 24 24 20
421►  6 10  6 38 10 14 18 12 16 12  2 22 24 42  8  4  2 60  6 10
441► 14 18 18 18 16 30 14  4  2 10  8 10 20 12 16 14  6 24 16  2
461► 12 10 18  2 24 34 12 14  6 10  6  2 10  8 28  2 10  2  6 10
481► 26 10  6 32 10 12  6  2 16 12 20 10 14  6 12 16 20  4  2 10
501► 14  4  6  2  4 14 16  8 36 10  2 12 16 20  4 12  6 30 38 16
521►  6 14  4  2 22  6 14 16  6  8 28  2  6 16  6 14  6 12 22 44
541►  6  4 24  2  6 28 14 22 20  4  6 36 14 18  6  4  6 26  4  2
561► 18 10  6  6  2  6  4  8 18 54 28 12  2  4 30 12  2  6 24 10
581► 12  6  8 10  6  8 16 12 14  6  4 18  8 10  2 12 30 16  2  6
601► 36 10 30  6 18  6  6  2 10 30  6 12 50 24  6  4  8 10 26  6
621►  4  2 18  4  2  6 10 12  2 24 16  6  2  6  4  8  4  6  8  6
641► 28 18  2  6 10  2 22 18 14 30 10 26 28  6 30  8  6 10  6  6
661►  2 10 36  2 12 10  6  6  6 14  6 10 20 12  6 24  6  6 28 18
681► 14  4 12 12 26 12 22 12  8 10  8 24 10  8 40  8  4 14  6 24
701►  4 18 12  6 20 22  2 16  6 20 16 30  8  6 18  6 22 18  2 18
721►  4  8 10  8 22  8  6 36 10 12  2  4 14 42 18 22  6 14  4  2
741► 10  2 42 10 18 30  2  6  4 14  6 10 14  4 18  2 16 14 10  2
761► 28  2 16  2 16 12 12  2 16 12  2 24 40  6  8  6  4 30  8 10
781► 14 18  6 16 18  6  2 18  4  6  6 26  4 26 28 26 24  4 32  6
                                        ╔════════════════╗
                                        ║ Collatz(38**8) ║
                                        ╚════════════════╝
4347792138496 2173896069248 1086948034624  543474017312  271737008656  135868504328   67934252164
  33967126082   16983563041   50950689124   25475344562   12737672281   38213016844   19106508422
   9553254211   28659762634   14329881317   42989643952   21494821976   10747410988    5373705494
   2686852747    8060558242    4030279121   12090837364    6045418682    3022709341    9068128024
   4534064012    2267032006    1133516003    3400548010    1700274005    5100822016    2550411008
   1275205504     637602752     318801376     159400688      79700344      39850172      19925086
      9962543      29887630      14943815      44831446      22415723      67247170      33623585
    100870756      50435378      25217689      75653068      37826534      18913267      56739802
     28369901      85109704      42554852      21277426      10638713      31916140      15958070
      7979035      23937106      11968553      35905660      17952830       8976415      26929246
     13464623      40393870      20196935      60590806      30295403      90886210      45443105
    136329316      68164658      34082329     102246988      51123494      25561747      76685242
     38342621     115027864      57513932      28756966      14378483      43135450      21567725
     64703176      32351588      16175794       8087897      24263692      12131846       6065923
     18197770       9098885      27296656      13648328       6824164       3412082       1706041
      5118124       2559062       1279531       3838594       1919297       5757892       2878946
      1439473       4318420       2159210       1079605       3238816       1619408        809704
       404852        202426        101213        303640        151820         75910         37955
       113866         56933        170800         85400         42700         21350         10675
        32026         16013         48040         24020         12010          6005         18016
         9008          4504          2252          1126           563          1690           845
         2536          1268           634           317           952           476           238
          119           358           179           538           269           808           404
          202           101           304           152            76            38            19
           58            29            88            44            22            11            34
           17            52            26            13            40            20            10
            5            16             8             4             2             1
                    ╔════════════════════════════════════════════════════╗
                    ║ sinD(-180, +180, 0.5);;; Plot DIGits 5 echoOptions ║
                    ╚════════════════════════════════════════════════════╝
│1                                                                  ∙∙∙∙∙∙
│                                                                 ∙∙∙    ∙∙∙
│                                                               ∙∙∙        ∙∙∙
│                                                              ∙∙            ∙∙
│                                                             ∙∙              ∙∙
│                                                            ∙∙                ∙∙
│                                                           ∙∙                  ∙∙
│                                                           ∙                    ∙
│                                                          ∙                      ∙
│                                                         ∙∙                      ∙∙
│                                                        ∙∙                        ∙∙
│                                                        ∙                          ∙
│                                                       ∙∙                          ∙∙
│                                                       ∙                            ∙∙
│                                                      ∙                              ∙
│                                                     ∙∙                              ∙∙
│                                                     ∙                                ∙
│                                                    ∙∙                                ∙∙
│                                                    ∙                                  ∙
│                                                   ∙∙                                   ∙
│                                                   ∙                                    ∙
│                                                  ∙                                      ∙
│                                                 ∙∙                                      ∙∙
│                                                 ∙                                        ∙
│                                                ∙∙                                        ∙∙
│                                                ∙                                          ∙
│                                               ∙∙                                          ∙∙
│                                               ∙                                            ∙
│0                                             ∙∙                                            ∙∙
∙──────────────────────────────────────────────∙──────────────────────────────────────────────∙
∙∙                                            ∙∙                                            721
│∙                                            ∙
│∙∙                                          ∙∙
│ ∙                                          ∙
│ ∙∙                                        ∙∙
│  ∙                                        ∙
│   ∙                                      ∙∙
│   ∙∙                                     ∙
│    ∙                                    ∙∙
│    ∙∙                                   ∙
│     ∙                                  ∙
│     ∙∙                                ∙∙
│      ∙                                ∙
│      ∙∙                              ∙∙
│       ∙                              ∙
│        ∙                            ∙∙
│        ∙∙                          ∙∙
│         ∙                          ∙
│          ∙                        ∙∙
│          ∙∙                      ∙∙
│           ∙∙                     ∙
│            ∙                    ∙∙
│            ∙∙                  ∙∙
│             ∙∙                ∙∙
│              ∙∙              ∙∙
│               ∙∙            ∙∙
│                ∙∙∙         ∙∙
│                  ∙∙∙    ∙∙∙
│-1                  ∙∙∙∙∙∙

time used for task 1 was    0 seconds.
time used for task 2 was    2 seconds.
time used for task 3 was    0 seconds.
time used for task 4 was    0 seconds.

```



## Ring


```ring

# Project : Rate counter

see "method 1: calculate reciprocal of elapsed time:" + nl
for trial = 1 to 3
    start = clock()
    tasktomeasure()
    finish = clock()
    see "rate = " + 100 / (finish-start) + " per second" + nl
next 

see "method 2: count completed tasks in one second:" + nl
for trial = 1 to 3
    runs = 0
    finish = clock() + 100
    while clock()  < finish
          tasktomeasure()
          if clock() < finish
             runs = runs + 1
          ok
    end
    see "rate = " + runs + " per second" + nl
next
 
func tasktomeasure
     for i = 1 to 100000
     next

```

Output:

```txt

method 1: calculate reciprocal of elapsed time:
rate = 6.67 per second
rate = 6.25 per second
rate = 6.67 per second
method 2: count completed tasks in one second:
rate = 5 per second
rate = 6 per second
rate = 5 per second

```



## Ruby

Testing lookup speed in array versus hash:

```ruby
require 'benchmark'
Document = Struct.new(:id,:a,:b,:c)
documents_a = []
documents_h = {}
1.upto(10_000) do |n|
  d = Document.new(n)
  documents_a << d
  documents_h[d.id] = d
end
searchlist = Array.new(1000){ rand(10_000)+1 }

Benchmark.bm(10) do |x|
  x.report('array'){searchlist.each{|el| documents_a.any?{|d| d.id == el}} }
  x.report('hash'){searchlist.each{|el| documents_h.has_key?(el)} }
end

```

{{Output}}

```txt

                 user     system      total        real
array       41.660000   0.000000  41.660000 ( 41.692570)
hash         0.020000   0.000000   0.020000 (  0.013756)

```



## Run BASIC


```runbasic
html "<table bgcolor=wheat border=1><tr><td align=center colspan=2>Rate Counter</td></tr>
    <tr><td>Run Job Times</td><td>"
    textbox #runTimes,"10",3

html "</tr><tr><td align=center colspan=2>"
     button #r,"Run", [runIt]
html "        "
     button #a, "Average", [ave]
html "</td></tr></table>"
wait

[runIt]
runTimes = min(10,val(#runTimes contents$()))
count = count + 1
print "-------- Run Number ";count;" ----------------"
print "Run jobs";runTimes;" times, reporting each"

for i = 1 to runTimes
    ' -----------------------------------------------------------------
    ' Normally we use a RUN() command to run another program
    ' but for test pruporse we have a routine that simply loops a bunch
    ' -----------------------------------------------------------------
    begTime  = time$("ms")
    theRun   = bogusProg()

    endTime  = time$("ms")
    lapsTime = endTime - begTime
    print "Job #";i;" Elapsed time, ms ";lapsTime;" ";1000/lapsTime; " ticks per second"
next
aveTime    = (endTime-startTime)/runTimes
totAveTime = totAveTime + aveTime
print "Average time, ms, is ";aveTime;" "; 1000/((endTime-startTime)/runTimes); " ticks per second"
wait

[ave]
print "---------------------------------"
print "Total average time:";aveTime/count

function bogusProg()
    for i = 1 to 10000
        sini = sini + sin(i)
        tani = tani + tan(i)
        cpsi = cosi + cos(i)
    next
end function 
```

Output:

<table bgcolor=wheat border=1><tr><td align=center colspan=2>Rate Counter</td></tr>
<tr><td>Run Job Times</td><td bgcolor=white>10</td></tr>
<tr><td align=center colspan=2>
<button value="Run"/>        
<button value="Average"/></td></tr></table>
.-------- Run Number 1 ----------------<br />
Run jobs 2 times, reporting each<br />
Job #1 Elapsed time, ms 50 20 ticks per second<br />
Job #2 Elapsed time, ms 48 20.8333349 ticks per second<br />
Average time, ms, is 1754768605184 5.69875717e-10 ticks per second<br />
.-------- Run Number 2 ----------------<br />
Run jobs 3 times, reporting each<br />
Job #1 Elapsed time, ms 47 21.2765955 ticks per second<br />
Job #2 Elapsed time, ms 47 21.2765955 ticks per second<br />
Job #3 Elapsed time, ms 47 21.2765955 ticks per second<br />
Average time, ms, is 1169845780480 8.54813575e-10 ticks per second<br />
.---------------------------------<br />
Total average time:584922890240<br />


## Scala

The solution below measures the number of tasks run in 5, 10 and 15 seconds. The tasks,
however, run multithreaded, not sequentially. It also does not stop the remaining tasks
once the time is up.


```scala
def task(n: Int) = Thread.sleep(n * 1000)
def rate(fs: List[() => Unit]) = {
  val jobs = fs map (f => scala.actors.Futures.future(f()))
  val cnt1 = scala.actors.Futures.awaitAll(5000, jobs: _*).count(_ != None)
  val cnt2 = scala.actors.Futures.awaitAll(5000, jobs: _*).count(_ != None)
  val cnt3 = scala.actors.Futures.awaitAll(5000, jobs: _*).count(_ != None)
  println("%d jobs in 5 seconds" format cnt1)
  println("%d jobs in 10 seconds" format cnt2)
  println("%d jobs in 15 seconds" format cnt3)
}
rate(List.fill(30)(() => task(scala.util.Random.nextInt(10)+1)))

```


The solution below runs a task repeatedly, for at most N seconds or Y times. The
precision available is milliseconds, though the sampling was limited to seconds. It
will wait until the current execution of the task is finished before announcing the
result, if the time runs out.


```scala
def rate(n: Int, y: Int)(task: => Unit) {
  val startTime = System.currentTimeMillis
  var currTime = startTime
  var loops = 0
  do {
    task
    currTime = System.currentTimeMillis
    loops += 1
  } while (currTime - startTime < n * 1000 && loops < y)
  if (currTime - startTime > n * 1000)
    println("Rate %d times per %d seconds" format (loops - 1, n))
  else
    println("Rate %d times in %.3f seconds" format (y, (currTime - startTime).toDouble / 1000))
}
rate(5, 20)(task(2))
```



## Sidef

{{trans|Perl}}

```ruby
var benchmark = frequire('Benchmark');

func job1 {
    #...job1 code...
}
func job2 {
    #...job2 code...
}

const COUNT = -1;   # run for one CPU second
benchmark.timethese(COUNT, Hash.new('Job1' => job1, 'Job2' => job2));
```



## Smalltalk

{{works with|Pharo}}
{{works with|Smalltalk/X}}

```smalltalk
|times|
times := Bag new.
1 to: 10 do: [:n| times add:
   (Time millisecondsToRun: [3000 factorial])].
Transcript show: times average asInteger.
```

Output:

```txt
153
```



## Tcl

The standard Tcl mechanism to measure how long a piece of code takes to execute is the <code>time</code> command. The first word of the string returned (which is also always a well-formed list) is the number of microseconds taken (in absolute time, not CPU time). Tcl uses the highest performance calibrated time source available on the system to compute the time taken; on Windows, this is derived from the system performance counter and not the (poor quality) standard system time source.

```tcl
set iters 10

# A silly example task
proc theTask {} {
    for {set a 0} {$a < 100000} {incr a} {
        expr {$a**3+$a**2+$a+1}
    }
}

# Measure the time taken $iters times
for {set i 1} {$i <= $iters} {incr i} {
    set t [lindex [time {
        theTask
    }] 0]
    puts "task took $t microseconds on iteration $i"
}
```

When tasks are are very quick, a more accurate estimate of the time taken can be gained by repeating the task many times between time measurements. In this next example, the task (a simple assignment) is repeated a million times between measures (this is very useful when performing performance analysis of the Tcl implementation itself).

```tcl
puts [time { set aVar 123 } 1000000]
```



## UNIX Shell

This code stores the number of times the program '''task''' can complete in 20 seconds. It is two parts.

Part 1: file "foo.sh"


This script spins, executing '''task''' as many times as possible.

```bash
#!/bin/bash

while : ; do
task && echo >> .fc
done
```


Part 2:


This script runs '''foo.sh''' in the background, and checks the rate count file every five seconds.  After four such checks, twenty seconds will have elapsed.

```bash
./foo.sh &
sleep 5
mv .fc .fc2 2>/dev/null
wc -l .fc2 2>/dev/null
rm .fc2
sleep 5
mv .fc .fc2 2>/dev/null
wc -l .fc2 2>/dev/null
sleep 5
mv .fc .fc2 2>/dev/null
wc -l .fc2 2>/dev/null
sleep 5
killall foo.sh
wc -l .fc 2>/dev/null
rm .fc
```



## XPL0


```XPL0
include c:\cxpl\codes;               \intrinsic 'code' declarations
int N, I, T0, Time;
[for N:= 1, 3 do
    [T0:= GetTime;
    for I:= 1 to 100 do
        [while port($3DA) & $08 do [];  \wait for vertical retrace to go away
        repeat until port($3DA) & $08;  \wait for vertical retrace signal
        ];
    Time:= GetTime - T0;
    IntOut(0, Time);  Text(0, " microseconds for 100 samples = ");  
    RlOut(0, 100.0e6/float(Time));  Text(0, "Hz");  CrLf(0);
    ];
]
```


Example output of vertical blanking (screen refresh) rates:

```txt

These measurements were taken while running under DOS 5.0 with the
output redirected to a file. When the output is instead displayed on the
screen, the second two readings consistently match for the first five
digits. The first reading varies because the program (purposely) does not
sync up on the retrace signal before starting the measurements.

1682024 microseconds for 100 samples =    59.45218Hz
1683980 microseconds for 100 samples =    59.38313Hz
1689918 microseconds for 100 samples =    59.17447Hz

These measurements were taken on a Windows XP machine running in windowed mode:

1384184 microseconds for 100 samples =    72.24473Hz
1355413 microseconds for 100 samples =    73.77825Hz
1326029 microseconds for 100 samples =    75.41313Hz

These measurements were taken on the same Windows machine in full-screen mode:

1418413 microseconds for 100 samples =    70.50133Hz
1424391 microseconds for 100 samples =    70.20544Hz
1395069 microseconds for 100 samples =    71.68104Hz

```



## Yabasic


```Yabasic
iterations = 100000

for j = 2 to 4
    a = peek("millisrunning")
    for i = 1 to iterations
        void = i + j^2
    next
    dif = peek("millisrunning") - a
    print "take ", dif, " ms";
    print " or ", iterations / dif * 1000 using "########", " sums per second"
next
```



## zkl

One second resolution and we let the computation run to completion. We could run the computation in a thread and signal it if time is up but that doesn't seem reasonable.

```zkl
fcn rateCounter(f,timeNRuns,secsToRun=Void){
   now:=Time.Clock.time; 
   if(secsToRun){
      then:=now + secsToRun;
      N:=0; do{ f(); N+=1; }while(Time.Clock.time<then);
      t:=Time.Clock.time - now;
      println("%d runs in %s seconds = %.3f sec/run"
              .fmt(N,Time.Date.toHMSString(0,0,t),t.toFloat()/N));
   }
   else{
      do(timeNRuns){ f() }
      t:=Time.Clock.time - now;
      println("%s seconds to run %d times = %.3f sec/run"
              .fmt(Time.Date.toHMSString(0,0,t),timeNRuns,
	           t.toFloat()/timeNRuns));
      t
   }
}
```


```zkl
ns:=List.createLong(0d100_000,(0).random,True); // one hundred thousand ints
rateCounter('wrap(){ ns.copy().sort() },20);
rateCounter('wrap(){ ns.copy().sort() },Void,10);
```

{{out}}

```txt

00:00:19 seconds to run 20 times = 0.950 sec/run
11 runs in 00:00:10 seconds = 0.909 sec/run

```

