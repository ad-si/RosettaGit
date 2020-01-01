+++
title = "Active object"
description = ""
date = 2019-06-21T00:00:50Z
aliases = []
[extra]
id = 3099
[taxonomies]
categories = []
tags = []
+++

{{task|Concurrency}}
[[Category:Object oriented]]{{requires|Concurrency}}{{requires|Objects}}{{requires|Mutable State}}

In [[object-oriented programming]] an object is active when its state depends on clock. Usually an active object encapsulates a [[task]] that updates the object's state. To the outer world the object looks like a normal object with methods that can be called from outside. Implementation of such methods must have a certain synchronization mechanism with the encapsulated task in order to prevent object's state corruption.

A typical instance of an active object is an animation widget. The widget state changes with the time, while as an object it has all properties of a normal widget.

'''The task'''

Implement an active integrator object. The object has an input and output. The input can be set using the method ''Input''. The input is a function of time. The output can be queried using the method ''Output''. The object integrates its input over the time and the result becomes the object's output. So if the input is ''K''(''t'') and the output is ''S'', the object state ''S'' is changed to ''S'' + (''K''(''t''<sub>1</sub>) + ''K''(''t''<sub>0</sub>)) * (''t''<sub>1</sub> - ''t''<sub>0</sub>) / 2, i.e. it integrates ''K'' using the trapeze method. Initially ''K'' is constant 0 and ''S'' is 0.

In order to test the object:
# set its input to sin (2π ''f t''), where the frequency ''f''=0.5Hz. The phase is irrelevant.
# wait 2s
# set the input to constant 0
# wait 0.5s

Verify that now the object's output is approximately 0 (the sine has the period of 2s). The accuracy of the result will depend on the [[OS]] scheduler time slicing and the accuracy of the clock.

{{omit from|VBScript}}

## Ada


```ada
with Ada.Calendar;                       use Ada.Calendar;
with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;

procedure Test_Integrator is
   type Func is access function (T : Time) return Float;

   function Zero (T : Time) return Float is
   begin
      return 0.0;
   end Zero;

   Epoch : constant Time := Clock;

   function Sine (T : Time) return Float is
   begin
      return Sin (Pi * Float (T - Epoch));
   end Sine;

   task type Integrator is
      entry Input  (Value : Func);
      entry Output (Value : out Float);
      entry Shut_Down;
   end Integrator;

   task body Integrator is
      K  : Func  := Zero'Access;
      S  : Float := 0.0;
      F0 : Float := 0.0;
      F1 : Float;
      T0 : Time  := Clock;
      T1 : Time;
   begin
      loop
         select
            accept Input (Value : Func) do
               K := Value;
            end Input;
         or accept Output (Value : out Float) do
               Value := S;
            end Output;
         or accept Shut_Down;
            exit;
         else
            T1 := Clock;
            F1 := K (T1);
            S  := S + 0.5 * (F1 + F0) * Float (T1 - T0);
            T0 := T1;
            F0 := F1;
         end select;
      end loop;
   end Integrator;

   I : Integrator;
   S : Float;
begin
   I.Input (Sine'Access);
   delay 2.0;
   I.Input (Zero'Access);
   delay 0.5;
   I.Output (S);
   Put_Line ("Integrated" & Float'Image (S) & "s");
   I.Shut_Down;
end Test_Integrator;
```

Sample output:

```txt

Integrated-5.34100E-05s

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"CLASSLIB"
      INSTALL @lib$+"TIMERLIB"
      INSTALL @lib$+"NOWAIT"

      REM Integrator class:
      DIM integ{f$, t#, v#, tid%, @init, @@exit, input, output, tick}
      PROC_class(integ{})

      REM Methods:
      DEF integ.@init integ.f$ = "0" : integ.tid% = FN_ontimer(10, PROC(integ.tick), 1) : ENDPROC
      DEF integ.@@exit PROC_killtimer(integ.tid%) : ENDPROC
      DEF integ.input (f$) integ.f$ = f$ : ENDPROC
      DEF integ.output = integ.v#
      DEF integ.tick integ.t# += 0.01 : integ.v# += EVAL(integ.f$) : ENDPROC

      REM Test:
      PROC_new(myinteg{}, integ{})
      PROC(myinteg.input) ("SIN(2*PI*0.5*myinteg.t#)")
      PROCwait(200)
      PROC(myinteg.input) ("0")
      PROCwait(50)
      PRINT "Final value = " FN(myinteg.output)
      PROC_discard(myinteg{})
```

Output:

```txt

Final value = -1.43349462E-6

```



## C

Uses POSIX threads.
{{libheader|pthread}}


```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>
#include <pthread.h>

/* no need to lock the object: at worst the readout would be 1 tick off,
   which is no worse than integrator's inate inaccuracy */
typedef struct {
	double (*func)(double);
	struct timeval start;
	double v, last_v, last_t;
	pthread_t id;
} integ_t, *integ;

void update(integ x)
{
	struct timeval tv;
	double t, v, (*f)(double);

	f = x->func;
	gettimeofday(&tv, 0);
	t = ((tv.tv_sec - x->start.tv_sec) * 1000000
		+ tv.tv_usec - x->start.tv_usec) * 1e-6;
	v = f ? f(t) : 0;
	x->v += (x->last_v + v) * (t - x->last_t) / 2;
	x->last_t = t;
}

void* tick(void *a)
{
	integ x = a;
	while (1) {
		usleep(100000); /* update every .1 sec */
		update(x);
	}
}

void set_input(integ x, double (*func)(double))
{
	update(x);
	x->func = func;
	x->last_t = 0;
	x->last_v = func ? func(0) : 0;
}

integ new_integ(double (*func)(double))
{
	integ x = malloc(sizeof(integ_t));
	x->v = x->last_v = 0;
	x->func = 0;
	gettimeofday(&x->start, 0);
	set_input(x, func);
	pthread_create(&x->id, 0, tick, x);
	return x;
}

double sine(double t) { return sin(4 * atan2(1, 1) * t); }

int main()
{
	integ x = new_integ(sine);
	sleep(2);
	set_input(x, 0);
	usleep(500000);
	printf("%g\n", x->v);

	return 0;
}
```

output

```txt
-9.99348e-05
```



## C#

{{works with|C# 6}}

```csharp
using System;
using System.Threading.Tasks;

using static System.Diagnostics.Stopwatch;
using static System.Math;
using static System.Threading.Thread;

class ActiveObject
{
    static double timeScale = 1.0 / Frequency;

    Func<double, double> func;
    Task updateTask;
    double integral;
    double value;
    long timestamp0, timestamp;

    public ActiveObject(Func<double, double> input)
    {
        timestamp0 = timestamp = GetTimestamp();
        func = input;
        value = func(0);
        updateTask = Integrate();
    }

    public void ChangeInput(Func<double, double> input)
    {
        lock (updateTask)
        {
            func = input;
        }
    }

    public double Value
    {
        get
        {
            lock (updateTask)
            {
                return integral;
            }
        }
    }

    async Task Integrate()
    {
        while (true)
        {
            await Task.Yield();
            var newTime = GetTimestamp();
            double newValue;

            lock (updateTask)
            {
                newValue = func((newTime - timestamp0) * timeScale);
                integral += (newValue + value) * (newTime - timestamp) * timeScale / 2;
            }

            timestamp = newTime;
            value = newValue;
        }
    }
}

class Program
{
    static Func<double, double> Sine(double frequency) =>
        t => Sin(2 * PI * frequency * t);

    static void Main(string[] args)
    {
        var ao = new ActiveObject(Sine(0.5));
        Sleep(TimeSpan.FromSeconds(2));
        ao.ChangeInput(t => 0);
        Sleep(TimeSpan.FromSeconds(0.5));
        Console.WriteLine(ao.Value);
    }
}
```


Output:

```txt
8.62230019255E-5
```



## C++

{{works with|C++14|}}

```cpp
#include <atomic>
#include <chrono>
#include <cmath>
#include <iostream>
#include <mutex>
#include <thread>

using namespace std::chrono_literals;

class Integrator
{
  public:
    using clock_type = std::chrono::high_resolution_clock;
    using dur_t      = std::chrono::duration<double>;
    using func_t     = double(*)(double);

    explicit Integrator(func_t f = nullptr);
    ~Integrator();
    void input(func_t new_input);
    double output() { return integrate(); }

  private:
    std::atomic_flag continue_;
    std::mutex       mutex;
    std::thread      worker;

    func_t                       func;
    double                       state = 0;
    //Improves precision by reducing sin result error on large values
    clock_type::time_point const beginning = clock_type::now();
    clock_type::time_point       t_prev = beginning;

    void do_work();
    double integrate();
};

Integrator::Integrator(func_t f) : func(f)
{
    continue_.test_and_set();
    worker = std::thread(&Integrator::do_work, this);
}

Integrator::~Integrator()
{
    continue_.clear();
    worker.join();
}

void Integrator::input(func_t new_input)
{
    integrate();
    std::lock_guard<std::mutex> lock(mutex);
    func = new_input;
}

void Integrator::do_work()
{
    while(continue_.test_and_set()) {
        integrate();
        std::this_thread::sleep_for(1ms);
    }
}

double Integrator::integrate()
{
    std::lock_guard<std::mutex> lock(mutex);
    auto now = clock_type::now();
    dur_t start = t_prev - beginning;
    dur_t fin   =    now - beginning;
    if(func)
        state += (func(start.count()) + func(fin.count())) * (fin - start).count() / 2;
    t_prev = now;
    return state;
}

double sine(double time)
{
    constexpr double PI = 3.1415926535897932;
    return std::sin(2 * PI * 0.5 * time);
}

int main()
{
    Integrator foo(sine);
    std::this_thread::sleep_for(2s);
    foo.input(nullptr);
    std::this_thread::sleep_for(500ms);
    std::cout << foo.output();
}
```

output

```txt
1.23136e-011
```



## Clojure


```clojure
(ns active-object
  (:import (java.util Timer TimerTask)))

(defn input [integrator k]
  (send integrator assoc :k k))

(defn output [integrator]
  (:s @integrator))

(defn tick [integrator t1]
  (send integrator
        (fn [{:keys [k s t0] :as m}]
          (assoc m :s (+ s (/ (* (+ (k t1) (k t0)) (- t1 t0)) 2.0)) :t0 t1))))

(defn start-timer [integrator interval]
  (let [timer (Timer. true)
        start (System/currentTimeMillis)]
    (.scheduleAtFixedRate timer
                          (proxy [TimerTask] []
                            (run [] (tick integrator (double (/ (- (System/currentTimeMillis) start) 1000)))))
                          (long 0)
                          (long interval))
    #(.cancel timer)))

(defn test-integrator []
  (let [integrator (agent {:k (constantly 0.0) :s 0.0 :t0 0.0})
        stop-timer (start-timer integrator 10)]
    (input integrator #(Math/sin (* 2.0 Math/PI 0.5 %)))
    (Thread/sleep 2000)
    (input integrator (constantly 0.0))
    (Thread/sleep 500)
    (println (output integrator))
    (stop-timer)))

user> (test-integrator)
1.414065859052494E-5

```


## D

{{trans|Java}}

```D
import core.thread;
import std.datetime;
import std.math;
import std.stdio;

void main() {
    auto func = (double t) => sin(cast(double) PI * t);
    Integrator integrator = new Integrator(func);
    Thread.sleep(2000.msecs);

    integrator.setFunc(t => 0.0);
    Thread.sleep(500.msecs);

    integrator.stop();
    writeln(integrator.getOutput());
}

/**
 * Integrates input function K over time
 * S + (t1 - t0) * (K(t1) + K(t0)) / 2
 */
public class Integrator {
    public alias Function = double function (double);

    private SysTime start;
    private shared bool running;

    private Function func;
    private shared double t0;
    private shared double v0;
    private shared double sum = 0.0;

    public this(Function func) {
        this.start = Clock.currTime();
        setFunc(func);
        new Thread({
            integrate();
        }).start();
    }

    public void setFunc(Function func) {
        this.func = func;
        v0 = func(0.0);
        t0 = 0.0;
    }

    public double getOutput() {
        return sum;
    }

    public void stop() {
        running = false;
    }

    private void integrate() {
        running = true;
        while (running) {
            Thread.sleep(1.msecs);
            update();
        }
    }

    private void update() {
        import core.atomic;

        Duration t1 = (Clock.currTime() - start);
        double v1 = func(t1.total!"msecs");
        double rect = (t1.total!"msecs" - t0) * (v0 + v1) / 2;
        atomicOp!"+="(this.sum, rect);
        t0 = t1.total!"msecs";
        v0 = v1;
    }
}
```


{{out}}

```txt
-3.07837e-13
```



## E



```e
def makeIntegrator() {
    var value := 0.0
    var input := fn { 0.0 }

    var input1 := input()
    var t1 := timer.now()

    def update() {
        def t2 := timer.now()
        def input2 :float64 := input()
        def dt := (t2 - t1) / 1000

        value += (input1 + input2) * dt / 2

        t1 := t2
        input1 := input2
    }

    var task() {
        update <- ()
        task <- ()
    }
    task()

    def integrator {
        to input(new) :void  { input := new }
        to output() :float64 { return value }
        to shutdown()        { task := fn {} }
    }
    return integrator
}

def test() {
    def result

    def pi := (-1.0).acos()
    def freq := pi / 1000

    def base := timer.now()
    def i := makeIntegrator()

    i.input(fn { (freq * timer.now()).sin() })
    timer.whenPast(base + 2000, fn {
        i.input(fn {0})
    })
    timer.whenPast(base + 2500, fn {
        bind result := i.output()
        i.shutdown()
    })
    return result
}
```



## EchoLisp

We use the functions (at ..) : scheduling, (wait ...), and (every ...) ot the timer.lib. The accuracy will be function of the browser's functions setTimeout and setInterval ...

```lisp

(require 'timer)

;; returns an 'object' : (&lamdba; message [values])
;; messages : input, output, sample, inspect
(define (make-active)
		(let [
		(t0 #f) (dt 0)
		(t  0) (Kt 0) ; K(t)
		(S  0) (K  0)]
		(lambda (message . args)
		(case message
			((output) (// S 2))
			((input ) (set! K (car args))  (set! t0 #f))
			((inspect) (printf " Active obj : t0 %v t %v S %v "  t0 t Kt (// S 2 )))
			((sample)
					(when (procedure? K)
;; recved new K : init
					(unless t0
						(set! t0  (first args))
						(set! t 0)
						(set! Kt (K 0)))

;; integrate K(t) every time 'sample message is received
					(set! dt (- (first args) t t0)) ;; compute once K(t)
					(set! S (+ S (* dt Kt)))
					(set! t (+ t dt))
					(set! Kt (K t))
					(set! S (+ S (* dt Kt)))))

			    (else (error "active:bad message" message))))))

```

{{Out}}

```lisp

(define (experiment)
	(define (K t) (sin (*  PI t )))
	(define A (make-active))
	(define (stop)  (A 'input 0))
	(define (sample t) (A 'sample (// t 1000)))
	(define (result) (writeln 'result (A 'output)))

	(at 2.5 'seconds 'result)
	(every 10 'sample) ;; integrate every 10 ms

	(A 'input K)
	(wait 2000 'stop))

(experiment) →
    3/7/2015 20:34:18 : result
    result     0.0002266920372221955
(experiment)  →
    3/7/2015 20:34:28 : result
    result     0.00026510586971023164

```



## Erlang

I could not see what time to use between each integration so it is the argument to task().

```Erlang

-module( active_object ).
-export( [delete/1, input/2, new/0, output/1, task/1] ).
-compile({no_auto_import,[time/0]}).

delete( Object ) ->
      Object ! stop.

input( Object, Fun ) ->
      Object ! {input, Fun}.

new( ) ->
      K = fun zero/1,
      S = 0,
      T0 = seconds_with_decimals(),
      erlang:spawn( fun() -> loop(K, S, T0) end ).

output( Object ) ->
      Object ! {output, erlang:self()},
      receive
      {output, Object, Output} -> Output
      end.

task( Integrate_millisec ) ->
      Object = new(),
      {ok, _Ref} = timer:send_interval( Integrate_millisec, Object, integrate ),
      io:fwrite( "New ~p~n", [output(Object)] ),
      input( Object, fun sine/1 ),
      timer:sleep( 2000 ),
      io:fwrite( "Sine ~p~n", [output(Object)] ),
      input( Object, fun zero/1 ),
      timer:sleep( 500 ),
      io:fwrite( "Approx ~p~n", [output(Object)] ),
      delete( Object ).



loop( Fun, Sum, T0 ) ->
      receive
      integrate ->
                T1 = seconds_with_decimals(),
                New_sum = trapeze( Sum, Fun, T0, T1 ),
                loop( Fun, New_sum, T1 );
      stop ->
                ok;
      {input, New_fun} ->
		loop( New_fun, Sum, T0 );
      {output, Pid} ->
                Pid ! {output, erlang:self(), Sum},
                loop( Fun, Sum, T0 )
      end.

sine( T ) ->
      math:sin( 2 * math:pi() * 0.5 * T ).

seconds_with_decimals() ->
      {Megaseconds, Seconds, Microseconds} = os:timestamp(),
      (Megaseconds * 1000000) + Seconds + (Microseconds / 1000000).

trapeze( Sum, Fun, T0, T1 ) ->
      Sum + (Fun(T1) + Fun(T0)) * (T1 - T0) / 2.

zero( _ ) -> 0.

```



## Factor

Working with dynamic quotations requires the stack effect to be known in advance. The apply-stack-effect serves this purpose.

```factor
USING: accessors alarms calendar combinators kernel locals math
math.constants math.functions prettyprint system threads ;
IN: rosettacode.active

TUPLE: active-object alarm function state previous-time ;

: apply-stack-effect ( quot -- quot' )
    [ call( x -- x ) ] curry ; inline

: nano-to-seconds ( -- seconds ) nano-count 9 10^ / ;

: object-times ( active-object -- t1 t2 )
    [ previous-time>> ]
    [ nano-to-seconds [ >>previous-time drop ] keep ] bi ;
:: adding-function ( t1 t2 active-object -- function )
    t2 t1 active-object function>> apply-stack-effect bi@ +
    t2 t1 - * 2 / [ + ] curry ;
: integrate ( active-object -- )
    [ object-times ]
    [ adding-function ]
    [ swap apply-stack-effect change-state drop ] tri ;

: <active-object> ( -- object )
    active-object new
    0 >>state
    nano-to-seconds >>previous-time
    [ drop 0 ] >>function
    dup [ integrate ] curry 1 nanoseconds every >>alarm ;
: destroy ( active-object -- ) alarm>> cancel-alarm ;

: input ( object quot -- object ) >>function ;
: output ( object -- val ) state>> ;

: active-test ( -- )
    <active-object>
    [ 2 pi 0.5 * * * sin ] input
    2 seconds sleep
    [ drop 0 ] input
    0.5 seconds sleep
    [ output . ] [ destroy ] bi ;
MAIN: active-test
```

    ( scratchpad ) "rosettacode.active" run
    -5.294207647335787e-05


## FBSL

The Dynamic Assembler and Dynamic C JIT compilers integrated in FBSL v3.5 handle multithreading perfectly well. However, pure FBSL infrastructure has never been designed especially to support own multithreading nor can it handle long long integers natively. Yet a number of tasks with careful design and planning are quite feasible in pure FBSL too:

```qbasic
#APPTYPE CONSOLE

#INCLUDE <Include\Windows.inc>

DIM Entity AS NEW Integrator(): Sleep(2000) ' respawn and do the job

Entity.Relax(): Sleep(500) ' get some rest

PRINT ">>> ", Entity.Yield(): DELETE Entity ' report and die

PAUSE

' ------------- End Program Code -------------

#DEFINE SpawnMutex CreateMutex(NULL, FALSE, "mutex")
#DEFINE LockMutex WaitForSingleObject(mutex, INFINITE)
#DEFINE UnlockMutex ReleaseMutex(mutex)
#DEFINE KillMutex CloseHandle(mutex)

CLASS Integrator

	PRIVATE:

	TYPE LARGE_INTEGER
		lowPart AS INTEGER
		highPart AS INTEGER
	END TYPE

	DIM dfreq AS DOUBLE, dlast AS DOUBLE, dnow AS DOUBLE, llint AS LARGE_INTEGER
	DIM dret0 AS DOUBLE, dret1 AS DOUBLE, mutex AS INTEGER, sum AS DOUBLE, thread AS INTEGER

	' --------------------------------------------
	SUB INITIALIZE()
		mutex = SpawnMutex
		QueryPerformanceFrequency(@llint)
		dfreq = LargeInt2Double(llint)
		QueryPerformanceCounter(@llint)
		dlast = LargeInt2Double(llint) / dfreq
		thread = FBSLTHREAD(ADDRESSOF Sampler)
		FBSLTHREADRESUME(thread)
	END SUB
	SUB TERMINATE()
		' nothing special
	END SUB
	' --------------------------------------------

	SUB Sampler()
		DO
			LockMutex
			Sleep(5)
			QueryPerformanceCounter(@llint)
			dnow = LargeInt2Double(llint) / dfreq
			dret0 = Task(dlast): dret1 = Task(dnow)
			sum = sum + (dret1 + dret0) * (dnow - dlast) / 2
			dlast = dnow
			UnlockMutex
		LOOP
	END SUB

	FUNCTION LargeInt2Double(obj AS VARIANT) AS DOUBLE
		STATIC ret
		ret = obj.highPart
		IF obj.highPart < 0 THEN ret = ret + (2 ^ 32)
		ret = ret * 2 ^ 32
		ret = ret + obj.lowPart
		IF obj.lowPart < 0 THEN ret = ret + (2 ^ 32)
		RETURN ret
	END FUNCTION

	PUBLIC:

	METHOD Relax()
		LockMutex
		ADDRESSOF Task = ADDRESSOF Idle
		UnlockMutex
	END METHOD

	METHOD Yield() AS DOUBLE
		LockMutex
		Yield = sum
		FBSLTHREADKILL(thread)
		UnlockMutex
		KillMutex
	END METHOD

END CLASS

FUNCTION Idle(BYVAL t AS DOUBLE) AS DOUBLE
	RETURN 0.0
END FUNCTION

FUNCTION Task(BYVAL t AS DOUBLE) AS DOUBLE
	RETURN SIN(2 * PI * 0.5 * t)
END FUNCTION
```


'''Typical console output:'''
 >>> -0.000769965989580346
 Press any key to continue...

=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Threading

// current time in seconds
let now() = float( DateTime.Now.Ticks / 10000L ) / 1000.0

type Integrator( intervalMs ) as x =
  let mutable k = fun _ -> 0.0  // function to integrate
  let mutable s = 0.0           // current value
  let mutable t0 = now()        // last time s was updated
  let mutable running = true    // still running?

  do x.ScheduleNextUpdate()

  member x.Input(f) = k <- f

  member x.Output() = s

  member x.Stop() = running <- false

  member private x.Update() =
    let t1 = now()
    s <- s + (k t0 + k t1) * (t1 - t0) / 2.0
    t0 <- t1
    x.ScheduleNextUpdate()

  member private x.ScheduleNextUpdate() =
    if running then
      async { do! Async.Sleep( intervalMs )
              x.Update()
            }
      |> Async.Start

let i = new Integrator(10)

i.Input( fun t -> Math.Sin (2.0 * Math.PI * 0.5 * t) )
Thread.Sleep(2000)

i.Input( fun _ -> 0.0 )
Thread.Sleep(500)

printfn "%f" (i.Output())
i.Stop()
```



## Go

Using time.Tick to sample K at a constant frequency.  Three goroutines are involved, main, aif, and tk.  Aif controls access to the accumulator s and the integration function K.  Tk and main must talk to aif through channels to access s and K.

```go
package main

import (
    "fmt"
    "math"
    "time"
)

// type for input function, k.
// input is duration since an arbitrary start time t0.
type tFunc func(time.Duration) float64

// active integrator object.  state variables are not here, but in
// function aif, started as a goroutine in the constructor.
type aio struct {
    iCh chan tFunc        // channel for setting input function
    oCh chan chan float64 // channel for requesting output
}

// constructor
func newAio() *aio {
    var a aio
    a.iCh = make(chan tFunc)
    a.oCh = make(chan chan float64)
    go aif(&a)
    return &a
}

// input method required by task description.  in practice, this method is
// unnecessary; you would just put that single channel send statement in
// your code wherever you wanted to set the input function.
func (a aio) input(f tFunc) {
    a.iCh <- f
}

// output method required by task description.  in practice, this method too
// would not likely be best.  instead any client interested in the value would
// likely make a return channel sCh once, and then reuse it as needed.
func (a aio) output() float64 {
    sCh := make(chan float64)
    a.oCh <- sCh
    return <-sCh
}

// integration function that returns constant 0
func zeroFunc(time.Duration) float64 { return 0 }

// goroutine serializes access to integrated function k and state variable s
func aif(a *aio) {
    var k tFunc = zeroFunc // integration function
    s := 0.                // "object state" initialized to 0
    t0 := time.Now()       // initial time
    k0 := k(0)             // initial sample value
    t1 := t0               // t1, k1 used for trapezoid formula
    k1 := k0

    tk := time.Tick(10 * time.Millisecond) // 10 ms -> 100 Hz
    for {
        select {
        case t2 := <-tk: // timer tick event
            k2 := k(t2.Sub(t0))                        // new sample value
            s += (k1 + k2) * .5 * t2.Sub(t1).Seconds() // trapezoid formula
            t1, k1 = t2, k2                            // save time and value
        case k = <-a.iCh: // input method event: function change
        case sCh := <-a.oCh: // output method event: sample object state
            sCh <- s
        }
    }
}

func main() {
    a := newAio()                           // create object
    a.input(func(t time.Duration) float64 { // 1. set input to sin function
        return math.Sin(t.Seconds() * math.Pi)
    })
    time.Sleep(2 * time.Second) // 2. sleep 2 sec
    a.input(zeroFunc)           // 3. set input to zero function
    time.Sleep(time.Second / 2) // 4. sleep .5 sec
    fmt.Println(a.output())     // output should be near zero
}
```

Output:

```txt

2.4517135756807704e-05

```



## Groovy

{{trans|Java}}

```groovy
/**
 * Integrates input function K over time
 * S + (t1 - t0) * (K(t1) + K(t0)) / 2
 */
class Integrator {
    interface Function {
        double apply(double timeSinceStartInSeconds)
    }

    private final long start
    private volatile boolean running

    private Function func
    private double t0
    private double v0
    private double sum

    Integrator(Function func) {
        this.start = System.nanoTime()
        setFunc(func)
        new Thread({ this.&integrate() }).start()
    }

    void setFunc(Function func) {
        this.func = func
        def temp = func.apply(0.0.toDouble())
        v0 = temp
        t0 = 0.0.doubleValue()
    }

    double getOutput() {
        return sum
    }

    void stop() {
        running = false
    }

    private void integrate() {
        running = true
        while (running) {
            try {
                Thread.sleep(1)
                update()
            } catch (InterruptedException ignored) {
                return
            }
        }
    }

    private void update() {
        double t1 = (System.nanoTime() - start) / 1.0e9
        double v1 = func.apply(t1)
        double rect = (t1 - t0) * (v0 + v1) / 2.0
        this.sum += rect
        t0 = t1
        v0 = v1
    }

    static void main(String[] args) {
        Integrator integrator = new Integrator({ t -> Math.sin(Math.PI * t) })
        Thread.sleep(2000)

        integrator.setFunc({ t -> 0.0.toDouble() })
        Thread.sleep(500)

        integrator.stop()
        System.out.println(integrator.getOutput())
    }
}
```

{{out}}

```txt
0.0039642136156300455
```



## Haskell



```haskell
module Integrator (
  newIntegrator, input, output, stop,
  Time, timeInterval
) where
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (evaluate)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- RC task
main = do let f = 0.5 {- Hz -}
          t0 <- getCurrentTime
          i <- newIntegrator
          input i (\t -> sin(2*pi * f * timeInterval t0 t)) -- task step 1
          threadDelay 2000000 {- µs -}                      -- task step 2
          input i (const 0)                                 -- task step 3
          threadDelay 500000 {- µs -}                       -- task step 4
          result <- output i
          stop i
          print result

---- Implementation ------------------------------------------------------

-- Utilities for working with the time type
type Time = UTCTime
type Func a = Time -> a
timeInterval t0 t1 = realToFrac $ diffUTCTime t1 t0

-- Type signatures of the module's interface
newIntegrator :: Fractional a => IO (Integrator a) -- Create an integrator
input  :: Integrator a -> Func a -> IO ()          -- Set the input function
output :: Integrator a           -> IO a           -- Get the current value
stop   :: Integrator a           -> IO ()          -- Stop integration, don't waste CPU

-- Data structures
data Integrator a = Integrator (MVar (IntState a)) -- MVar is a thread-safe mutable cell
  deriving Eq
data IntState a = IntState { func  :: Func a,      -- The current function
                             run   :: Bool,        -- Whether to keep going
                             value :: a,           -- The current accumulated value
                             time  :: Time }       -- The time of the previous update

newIntegrator = do
  now <- getCurrentTime
  state <- newMVar $ IntState { func  = const 0,
                                run   = True,
                                value = 0,
                                time  = now }
  thread <- forkIO (intThread state)  -- The state variable is shared between the thread
  return (Integrator state)           --   and the client interface object.

input  (Integrator stv) f = modifyMVar_ stv (\st -> return st { func = f })
output (Integrator stv)   = fmap value $ readMVar stv
stop   (Integrator stv)   = modifyMVar_ stv (\st -> return st { run = False })
  -- modifyMVar_ takes an MVar and replaces its contents according to the provided function.
  -- a { b = c } is record-update syntax: "the record a, except with field b changed to c"

-- Integration thread
intThread :: Fractional a => MVar (IntState a) -> IO ()
intThread stv = whileM $ modifyMVar stv updateAndCheckRun
  -- modifyMVar is like modifyMVar_ but the function returns a tuple of the new value
  -- and an arbitrary extra value, which in this case ends up telling whileM whether
  -- to keep looping.
  where updateAndCheckRun st = do
          now <- getCurrentTime
          let value' = integrate (func st) (value st) (time st) now
          evaluate value'                             -- avoid undesired laziness
          return (st { value = value', time  = now }, -- updated state
                  run st)                             -- whether to continue

integrate :: Fractional a => Func a -> a -> Time -> Time -> a
integrate f value t0 t1 = value + (f t0 + f t1)/2 * dt
  where dt = timeInterval t0 t1

-- Execute 'action' until it returns false.
whileM action = do b <- action; if b then whileM action else return ()
```



## J


Implementation:


```J
coclass 'activeobject'
require'dates'

create=:setinput NB. constructor

T=:3 :0
  if. nc<'T0' do. T0=:tsrep 6!:0'' end.
  0.001*(tsrep 6!:0'')-T0
)

F=:G=:0:
Zero=:0

setinput=:3 :0
  zero=. getoutput''
  '`F ignore'=: y,_:`''
  G=: F f.d._1
  Zero=: zero-G T ''
  getoutput''
)

getoutput=:3 :0
  Zero+G T''
)
```


Task example (code):


```J
cocurrent 'testrig'

delay=: 6!:3

object=: conew 'activeobject'
setinput__object 1&o.@o.`''
smoutput (T__object,getoutput__object) ''

delay 2

smoutput (T__object,getoutput__object) ''
setinput__object 0:`''
smoutput (T__object,getoutput__object) ''

delay 0.5

smoutput (T__object,getoutput__object) ''
```


Task example (output):


```txt
0.001 0
2.002 4.71237e_6
2.004 1.25663e_5
2.504 1.25663e_5
```


First column is time relative to start of processing, second column is object's output at that time.


## Java


```java
/**
 * Integrates input function K over time
 * S + (t1 - t0) * (K(t1) + K(t0)) / 2
 */
public class Integrator {

    public interface Function {
        double apply(double timeSinceStartInSeconds);
    }

    private final long start;
    private volatile boolean running;

    private Function func;
    private double t0;
    private double v0;
    private double sum;

    public Integrator(Function func) {
        this.start = System.nanoTime();
        setFunc(func);
        new Thread(this::integrate).start();
    }

    public void setFunc(Function func) {
        this.func = func;
        v0 = func.apply(0.0);
        t0 = 0;
    }

    public double getOutput() {
        return sum;
    }

    public void stop() {
        running = false;
    }

    private void integrate() {
        running = true;
        while (running) {
            try {
                Thread.sleep(1);
                update();
            } catch (InterruptedException e) {
                return;
            }
        }
    }

    private void update() {
        double t1 = (System.nanoTime() - start) / 1.0e9;
        double v1 = func.apply(t1);
        double rect = (t1 - t0) * (v0 + v1) / 2;
        this.sum += rect;
        t0 = t1;
        v0 = v1;
    }

    public static void main(String[] args) throws InterruptedException {
        Integrator integrator = new Integrator(t -> Math.sin(Math.PI * t));
        Thread.sleep(2000);

        integrator.setFunc(t -> 0.0);
        Thread.sleep(500);

        integrator.stop();
        System.out.println(integrator.getOutput());
    }
}

```

Output:

```txt
4.783602720556498E-13
```



## JavaScript


{{trans|E}}


```javascript
function Integrator(sampleIntervalMS) {
    var inputF = function () { return 0.0 };
    var sum = 0.0;

    var t1 = new Date().getTime();
    var input1 = inputF(t1 / 1000);

    function update() {
        var t2 = new Date().getTime();
        var input2 = inputF(t2 / 1000);
        var dt = (t2 - t1) / 1000;

        sum += (input1 + input2) * dt / 2;

        t1 = t2;
        input1 = input2;
    }

    var updater = setInterval(update, sampleIntervalMS);

    return ({
        input: function (newF) { inputF = newF },
        output: function () { return sum },
        shutdown: function () { clearInterval(updater) },
    });
}
```


Test program as a HTML fragment:


```html4strict><p
<span id="a">Test running...</span> <code id="b">-</code></p>

<script type="text/javascript">
    var f = 0.5;

    var i = new Integrator(1);
    var displayer = setInterval(function () { document.getElementById("b").firstChild.data = i.output() }, 100)

    setTimeout(function () {
        i.input(function (t) { return Math.sin(2*Math.PI*f*t) }); // test step 1
        setTimeout(function () { // test step 2
            i.input(function (t) { return 0 }); // test step 3
            setTimeout(function () { // test step 3
                i.shutdown();
                clearInterval(displayer);
                document.getElementById("a").firstChild.data = "Done, should be about 0: "
            }, 500);
        }, 2000);
    }, 1)
</script>
```



## Julia

{{works with|Julia|0.6}}
Julia has inheritance of data structures and first-class types, but structures do not have methods.
Instead, methods are functions with multiple dispatch based on argument type.

```julia
mutable struct Integrator
    func::Function
    runningsum::Float64
    dt::Float64
    running::Bool
    function Integrator(f::Function, dt::Float64)
        this = new()
        this.func = f
        this.runningsum = 0.0
        this.dt = dt
        this.running = false
        return this
    end
end

function run(integ::Integrator, lastval::Float64 = 0.0)
    lasttime = time()
    while integ.running
        sleep(integ.dt)
        newtime = time()
        measuredinterval = newtime - lasttime
        newval = integ.func(measuredinterval)
        integ.runningsum += (lastval + newval) * measuredinterval / 2.0
        lasttime = newtime
        lastval = newval
    end
end

start!(integ::Integrator) = (integ.running = true; @async run(integ))
stop!(integ) = (integ.running = false)
f1(t) = sin(2π * t)
f2(t) = 0.0

it = Integrator(f1, 0.00001)
start!(it)
sleep(2.0)
it.func = f2
sleep(0.5)
v2 = it.runningsum
println("After 2.5 seconds, integrator value was $v2")
```



## Kotlin

{{trans|Java}}
Athough this is a faithful translation of the Java entry, on my machine the output of the latter is typically an order of magnitude smaller than this version. I have no idea why.

```scala
// version 1.2.0

import kotlin.math.*

typealias Function = (Double) -> Double

/**
 * Integrates input function K over time
 * S + (t1 - t0) * (K(t1) + K(t0)) / 2
 */
class Integrator {
    private val start: Long
    private @Volatile var running = false
    private lateinit var func: Function
    private var t0 = 0.0
    private var v0 = 0.0
    private var sum = 0.0

    constructor(func: Function) {
        start = System.nanoTime()
        setFunc(func)
        Thread(this::integrate).start()
    }

    fun setFunc(func: Function) {
        this.func = func
        v0 = func(0.0)
        t0 = 0.0
    }

    fun getOutput() = sum

    fun stop() {
        running = false
    }

    private fun integrate() {
        running = true
        while (running) {
            try {
                Thread.sleep(1)
                update()
            }
            catch(e: InterruptedException) {
                return
            }
        }
    }

    private fun update() {
        val t1 = (System.nanoTime() - start) / 1.0e9
        val v1 = func(t1)
        val rect = (t1 - t0) * (v0 + v1) / 2.0
        sum  += rect
        t0 = t1
        v0 = v1
    }
}

fun main(args: Array<String>) {
    val integrator = Integrator( { sin(PI * it) } )
    Thread.sleep(2000)

    integrator.setFunc( { 0.0 } )
    Thread.sleep(500)

    integrator.stop()
    println(integrator.getOutput())
}
```


Sample output:

```txt

2.884266305153741E-4

```



## Lingo

Parent script "Integrator":

```Lingo
property _sum
property _func
property _timeLast
property _valueLast
property _ms0
property _updateTimer

on new (me, func)
    if voidP(func) then func = "0.0"
    me._sum = 0.0
    -- update frequency: 100/sec (arbitrary)
    me._updateTimer = timeout().new("update", 10, #_update, me)
    me.input(func)
    return me
end

on stop (me)
    me._updateTimer.period = 0 -- deactivates timer
end

-- func is a term (as string) that might contain "t" and is evaluated at runtime
on input (me, func)
    me._func = func
    me._ms0 = _system.milliseconds
    me._timeLast = 0.0
    t = 0.0
    me._valueLast = value(me._func)
end

on output (me)
    return me._sum
end

on _update (me)
    now = _system.milliseconds - me._ms0
    t = now/1000.0
    val = value(me._func)
    me._sum = me._sum + (me._valueLast+val)*(t - me._timeLast)/2
    me._timeLast = t
    me._valueLast = val
end
```


In some movie script:

```Lingo
global gIntegrator

-- entry point
on startMovie
    gIntegrator = script("Integrator").new("sin(PI * t)")
    timeout().new("timer", 2000, #step1)
end

on step1 (_, timer)
    gIntegrator.input("0.0")
    timer.timeoutHandler = #step2
    timer.period = 500
end

on step2 (_, timer)
    gIntegrator.stop()
    put gIntegrator.output()
    timer.forget()
end
```


{{out}}

```txt
-- 0.0004
```



## Mathematica


```Mathematica
Block[{start = SessionTime[], K, t0 = 0, t1, kt0, S = 0},
 K[t_] = Sin[2 Pi f t] /. f -> 0.5; kt0 = K[t0];
 While[True, t1 = SessionTime[] - start;
  S += (kt0 + (kt0 = K[t1])) (t1 - t0)/2; t0 = t1;
  If[t1 > 2, K[t_] = 0; If[t1 > 2.5, Break[]]]]; S]
```


 1.1309*10^-6

Curiously, this value never changes; it is always exactly the same (at 1.1309E-6). Note that closer answers could be achieved by using Mathematica's better interpolation methods, but it would require collecting the data (in a list), which would have a speed penalty large enough to negate the improved estimation.


## ooRexx

Not totally certain this is a correct implementation since the value coming out is not close to zero.  It does show all of the basics of multithreading and object synchronization though.


```ooRexx

integrater = .integrater~new(.routines~sine)   -- start the integrater function
call syssleep 2
integrater~input = .routines~zero              -- update the integrater function
call syssleep .5

say integrater~output
integrater~stop          -- terminate the updater thread

::class integrater
::method init
  expose stopped start v last_v last_t k
  use strict arg k
  stopped = .false
  start = .datetime~new   -- initial time stamp
  v = 0
  last_v = 0
  last_t = 0
  self~input = k
  self~start

-- spin off a new thread and start updating.  Note, this method is unguarded
-- to allow other threads to make calls
::method start unguarded
  expose stopped

  reply  -- this spins this method invocation off onto a new thread

  do while \stopped
    call sysSleep .1
    self~update    -- perform the update operation
  end

-- turn off the thread.  Since this is unguarded,
-- it can be called any time, any where
::method stop unguarded
  expose stopped
  stopped = .true

-- perform the update.  Since this is a guarded method, the object
-- start is protected.
::method update
  expose start v last_v t last_t k

  numeric digits 20   -- give a lot of precision

  current = .datetime~new
  t = (current - start)~microseconds
  new_v = k~call(t)    -- call the input function
  v += (last_v + new_v) * (t - last_t) / 2
  last_t = t
  last_v = new_v
  say new value is v

-- a write-only attribute setter (this is GUARDED)
::attribute input SET
  expose k last_t last_v
  self~update          -- update current values
  use strict arg k  -- update the call function to the provided value
  last_t = 0
  last_v = k~call(0)  -- and update to the zero value

-- the output function...returns current calculated value
::attribute output GET
  expose v
  return v

::routine zero
  return 0

::routine sine
  use arg t
  return rxcalcsin(rxcalcpi() * t)

::requires rxmath library


```



## OxygenBasic

Built from scratch. The ringmaster orchestrates all the active-objects, keeping a list of
each individual and its method call.

With a high precision timer the result is around -.0002

```oxygenbasic

double MainTime

'
### =========

class RingMaster
'
### =========

'
indexbase 1
sys List[512] 'limit of 512 objects per ringmaster
sys max,acts
'
method Register(sys meth,obj) as sys
  sys i
  for i=1 to max step 2
    if list[i]=0 then exit for 'vacant slot
  next
  if i>=max then max+=2
  List[i]<=meth,obj
  return i 'token for deregistration etc
end method
'
method Deregister(sys *i)
  if i then List[i]<=0,0 : i=0
end method
'
method Clear()
  max=0
end method
'
method Act() 'called by the timer
  sys i,q
  for i=1 to max step 2
    q=List[i]
    if q then
      call q List[i+1] 'anon object
    end if
  next
  acts++
end method
'
end class


'
### ===========

class ActiveObject
'
### ===========

'
double     s,freq,t1,t2,v1,v2
sys        nfun,acts,RingToken
RingMaster *Master
'
method fun0() as double
end method
'
method fun1() as double
  return sin(2*pi()*freq*MainTime)
end method
'
method func() as double
  select case nfun
    case 0 : return fun0()
    case 1 : return fun1()
  end select
  'error?
end method
'
method TimeBasedDuties()
  t1=t2
  v1=v2
  t2=MainTime
  v2=func
  s=s+(v2+v1)*(t2-t1)*0.5 'add slice to integral
  acts++
end method
'
method RegisterWith(RingMaster*r)
  @Master=@r
  if @Master then
    RingToken=Master.register @TimeBasedDuties,@this
  end if
end method
'
method Deregister()
  if @Master then
    Master.Deregister RingToken 'this is set to null
  end if
end method
'
method Output() as double
  return s
end method
'
method Input(double fr=0,fun=0)
  if fr then freq=fr
  nfun=fun
end method

method ClearIntegral()
  s=0
end method
'
end class


'SETUP TIMING SYSTEM
'
### =============


extern library "kernel32.dll"
declare QueryPerformanceCounter (quad*c)
declare QueryPerformanceFrequency(quad*f)
declare Sleep(sys milliseconds)
end extern
'
quad scount,tcount,freq
QueryPerformanceFrequency freq
double tscale=1/freq
double t1,t2
QueryPerformanceCounter scount

macro PrecisionTime(time)
  QueryPerformanceCounter tcount
  time=(tcount-scount)*tscale
end macro


'====
'TEST
'====

double       integral
double       tevent1,tevent2
RingMaster   Rudolpho
ActiveObject A
'
A.RegisterWith Rudolpho
A.input (fr=0.5, fun=1) 'start with the freqency function (1)
'
'SET EVENT TIMES
'
### =========


tEvent1=2.0 'seconds
tEvent2=2.5 'seconds
'
PrecisionTime t1 'mark initial time
MainTime=t1
'
'
'EVENT LOOP
'
### ====

'
do
  PrecisionTime t2
  MainTime=t2
  if t2-t1>=0.020 'seconds interval
    Rudolpho.Act 'service all active objects
    t1=t2
  end if
  '
  if tEvent1>=0 and MainTime>=tEvent1
    A.input (fun=0) 'switch to null function (0)
    tEvent1=-1      'disable this event from happening again
  end if
  if MainTime>=tEvent2
    integral=A.output()
    exit do 'end of session
  end if
  '
  sleep 5 'hand control to OS for a while
end do

print str(integral,4)

Rudolpho.clear
```



## Oz


```oz
declare
  fun {Const X}
     fun {$ _} X end
  end

  fun {Now}
     {Int.toFloat {Property.get 'time.total'}} / 1000.0
  end

  class Integrator from Time.repeat
     attr
        k:{Const 0.0}
        s:0.0
        t1 k_t1
        t2 k_t2

     meth init(SampleIntervalMS)
        t1 := {Now}
        k_t1 := {@k @t1}
        {self setRepAll(action:Update
                        delay:SampleIntervalMS)}
        thread
           {self go}
        end
     end

     meth input(K)
        k := K
     end

     meth output($)
        @s
     end

     meth Update
        t2 := {Now}
        k_t2 := {@k @t2}
        s := @s + (@k_t1 + @k_t2) * (@t2 - @t1) / 2.0
        t1 := @t2
        k_t1 := @k_t2
     end
  end

  Pi = 3.14159265
  F = 0.5

  I = {New Integrator init(10)}
in
  {I input(fun {$ T}
              {Sin 2.0 * Pi * F * T}
           end)}

  {Delay 2000} %% ms

  {I input({Const 0.0})}

  {Delay 500} %% ms

  {Show {I output($)}}
  {I stop}
```



## Perl


```perl
#!/usr/bin/perl

use strict;
use 5.10.0;

package Integrator;
use threads;
use threads::shared;

sub new {
	my $cls = shift;
	my $obj = bless {	t	=> 0,
				sum	=> 0,
				ref $cls ? %$cls : (),
				stop	=> 0,
				tid	=> 0,
				func	=> shift,
			}, ref $cls || $cls;

	share($obj->{sum});
	share($obj->{stop});

	$obj->{tid} = async {
		my $upd = 0.1; # update every 0.1 second
		while (!$obj->{stop}) {
			{
				my $f = $obj->{func};
				my $t = $obj->{t};

				$obj->{sum} += ($f->($t) + $f->($t + $upd))* $upd/ 2;
				$obj->{t} += $upd;
			}
			select(undef, undef, undef, $upd);
		}
	#	say "stopping $obj";
	};
	$obj
}

sub output { shift->{sum} }

sub delete {
	my $obj = shift;
	$obj->{stop} = 1;
	$obj->{tid}->join;
}

sub setinput {
	# This is surprisingly difficult because of the perl sharing model.
	# Func refs can't be shared, thus can't be replaced by another thread.
	# Have to create a whole new object... there must be a better way.
	my $obj = shift;
	$obj->delete;
	$obj->new(shift);
}

package main;

my $x = Integrator->new(sub { sin(atan2(1, 1) * 8 * .5 * shift) });

sleep(2);
say "sin after 2 seconds: ", $x->output;

$x = $x->setinput(sub {0});

select(undef, undef, undef, .5);
say "0 after .5 seconds: ", $x->output;

$x->delete;
```



## Perl 6

{{works with|Rakudo|2018.12}}
There is some jitter in the timer, but it is typically accurate to within a few thousandths of a second.


```perl6
class Integrator {
    has $.f is rw = sub ($t) { 0 };
    has $.now is rw;
    has $.value is rw = 0;
    has $.integrator is rw;

    method init() {
        self.value = &(self.f)(0);
        self.integrator = Thread.new(
            :code({
                loop {
                    my $t1 = now;
                    self.value += (&(self.f)(self.now) + &(self.f)($t1)) * ($t1 - self.now) / 2;
                    self.now = $t1;
                    sleep .001;
                }
            }),
            :app_lifetime(True)
        ).run
    }

    method Input (&f-of-t) {
        self.f = &f-of-t;
        self.now = now;
        self.init;
    }

    method Output { self.value }
}

my $a = Integrator.new;

$a.Input( sub ($t) { sin(2 * π * .5 * $t) } );

say "Initial value: ", $a.Output;

sleep 2;

say "After 2 seconds: ", $a.Output;

$a.Input( sub ($t) { 0 } );

sleep .5;

say "f(0): ", $a.Output;
```


{{out|Typical output}}

```txt
Initial value: 0
After 2 seconds: -0.0005555887464620366
f(0): 0
```



## Phix

Note that in Phix you cannot pass a variable to another procedure and have it "change under your feet".

The copy-on-write semantics mean it would not have any effect, in that the original would be preserved
(deemed in phix to be a "very good thing") while the value passed along, a shared reference until it gets
modified and a copy made, would most likely simply be discarded, unless explicitly returned and stored,
which obviously cannot be done from a separate thread.
Instead we pass around an index (dx) as a way of emulating the "pointer references" of other languages.

If anything phix requires more locking that other languages due to the hidden shared reference counts.

Just lock everything, it is not that hard, and you should never need much more than the stuff below.

```Phix
sequence x = {}
enum TERMINATE, INTERVAL, KFUN, VALUE, T0, K0, ID, ISIZE=$
integer xlock = init_cs()

function zero(atom /*t*/) return 0 end function
function sine(atom t) return sin(2*PI*0.5*t) end function

procedure update(integer dx)
    enter_cs(xlock)
    atom t1 = time(),
         k1 = call_func(x[dx][KFUN],{t1})
    x[dx][VALUE] += (k1 + x[dx][K0]) * (t1 - x[dx][T0]) / 2
    x[dx][T0] = t1
    x[dx][K0] = k1
    leave_cs(xlock)
end procedure

procedure tick(integer dx)
    while not x[dx][TERMINATE] do
        sleep(x[dx][INTERVAL])
        update(dx)
    end while
end procedure

function new_integrator(integer rid, atom interval)
    x = append(x,repeat(0,ISIZE))
    integer dx = length(x)
    x[dx][TERMINATE] = false
    x[dx][INTERVAL] = interval
    x[dx][KFUN] = rid
    x[dx][T0] = time()
    update(dx)
    x[dx][ID] = create_thread(routine_id("tick"),{dx})
    return dx
end function

procedure set_input(integer dx, rid)
    enter_cs(xlock)
    x[dx][KFUN] = rid
    x[dx][K0] = 0
    leave_cs(xlock)
end procedure

function get_output(integer dx)
    enter_cs(xlock)
    atom v = x[dx][VALUE]
    leave_cs(xlock)
    return v
end function

procedure stop_integrator(integer dx)
    x[dx][TERMINATE] = true
    wait_thread(x[dx][ID])
end procedure

puts(1,"")
integer dx = new_integrator(routine_id("sine"),0.01)
sleep(2)
printf(1,"%f\n",get_output(dx))
set_input(dx,routine_id("zero"))
sleep(0.5)
printf(1,"%f\n",get_output(dx))
stop_integrator(dx)
```

{{out}}

```txt

-0.00326521
0.00196980

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(class +Active)
# inp val sum usec

(dm T ()
   (unless (assoc -100 *Run)           # Install timer task
      (task -100 100                   # Update objects every 0.1 sec
         (mapc 'update> *Actives) ) )
   (=: inp '((U) 0))                   # Set zero input function
   (=: val 0)                          # Initialize last value
   (=: sum 0)                          # Initialize sum
   (=: usec (usec))                    # and time
   (push '*Actives This) )             # Install in notification list

(dm input> (Fun)
   (=: inp Fun) )

(dm update> ()
   (let (U (usec)  V ((: inp) U))      # Get current time, calculate value
      (inc (:: sum)
         (*/
            (+ V (: val))              # (K(t[1]) + K(t[0])) *
            (- U (: usec))             # (t[1] - t[0]) /
            2.0 ) )                    # 2.0
      (=: val V)
      (=: usec U) ) )

(dm output> ()
   (format (: sum) *Scl) )             # Get result

(dm stop> ()
   (unless (del This '*Actives)        # Removing the last active object?
      (task -100) ) )                  # Yes: Uninstall timer task

(de integrate ()                       # Test it
   (let Obj (new '(+Active))           # Create an active object
      (input> Obj                      # Set input function
         '((U) (sin (*/ pi U 1.0))) )  # to sin(π * t)
      (wait 2000)                      # Wait 2 sec
      (input> Obj '((U) 0))            # Reset input function
      (wait 500)                       # Wait 0.5 sec
      (prinl "Output: " (output> Obj)) # Print return value
      (stop> Obj) ) )                  # Stop active object
```



## PureBasic

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimpleOOP].

```PureBasic
Prototype.d ValueFunction(f.d, t.d)

Class IntegralClass
  Time0.i
  Mutex.i
  S.d
  Freq.d
  Thread.i
  Quit.i
  *func.ValueFunction

  Protect Method Sampler()
    Repeat
      Delay(1)
      If This\func And This\Mutex
        LockMutex(This\Mutex)
        This\S + This\func(This\Freq, ElapsedMilliseconds()-This\Time0)
        UnlockMutex(This\Mutex)
      EndIf
    Until This\Quit
  EndMethod

  BeginPublic
    Method Input(*func.ValueFunction)
      LockMutex(This\Mutex)
      This\func = *func
      UnlockMutex(This\Mutex)
    EndMethod

    Method.d Output()
      Protected Result.d
      LockMutex(This\Mutex)
      Result = This\S
      UnlockMutex(This\Mutex)
      MethodReturn Result
    EndMethod

    Method Init(F.d, *f)
      This\Freq   = F
      This\func   = *f
      This\Mutex  = CreateMutex()
      This\Time0  = ElapsedMilliseconds()
      This\Thread = CreateThread(This\Sampler, This)
      ThreadPriority(This\Thread, 10)
    EndMethod

    Method Release()
      This\Quit = #True
      WaitThread(This\Thread)
    EndMethod
  EndPublic

EndClass

;- Procedures for generating values
Procedure.d n(f.d, t.d)
  ; Returns nothing
EndProcedure

Procedure.d f(f.d, t.d)
  ; Returns the function of this task
  ProcedureReturn Sin(2*#PI*f*t)
EndProcedure

;- Test Code
*a.IntegralClass = NewObject.IntegralClass(0.5, @n()) ; Create the AO
*a\Input(@f()) ; Start sampling function f()
Delay(2000)    ; Delay 2 sec
*a\Input(@n()) ; Change to sampling 'nothing'
Delay( 500)    ; Wait 1/2 sec
MessageRequester("Info", StrD(*a\Output()))           ; Present the result
*a= FreeObject
```



## Python

Assignment is thread-safe in Python, so no extra locks are needed in this case.



```python
from time import time, sleep
from threading import Thread

class Integrator(Thread):
    'continuously integrate a function `K`, at each `interval` seconds'
    def __init__(self, K=lambda t:0, interval=1e-4):
        Thread.__init__(self)
        self.interval  = interval
        self.K   = K
        self.S   = 0.0
        self.__run = True
        self.start()

    def run(self):
        "entry point for the thread"
        interval = self.interval
        start = time()
        t0, k0 = 0, self.K(0)
        while self.__run:
            sleep(interval)
            t1 = time() - start
            k1 = self.K(t1)
            self.S += (k1 + k0)*(t1 - t0)/2.0
            t0, k0 = t1, k1

    def join(self):
        self.__run = False
        Thread.join(self)

if __name__ == "__main__":
    from math import sin, pi

    ai = Integrator(lambda t: sin(pi*t))
    sleep(2)
    print ai.S
    ai.K = lambda t: 0
    sleep(0.5)
    print ai.S
```



## Racket



```racket

#lang racket

(require (only-in racket/gui sleep/yield timer%))

(define active%
  (class object%
    (super-new)
    (init-field k) ; input function
    (field [s 0])  ; state
    (define t_0 0)

    (define/public (input new-k) (set! k new-k))
    (define/public (output) s)

    (define (callback)
      (define t_1 (/ (- (current-inexact-milliseconds) start) 1000))
      (set! s (+ s (* (+ (k t_0) (k t_1))
                      (/ (- t_1 t_0) 2))))
      (set! t_0 t_1))

    (define start (current-inexact-milliseconds))
    (new timer%
         [interval 1000]
         [notify-callback callback])))

(define active (new active% [k (λ (t) (sin (* 2 pi 0.5 t)))]))
(sleep/yield 2)
(send active input (λ _ 0))
(sleep/yield 0.5)
(displayln (send active output))

```


## Rust



```rust
#![feature(mpsc_select)]

extern crate num;
extern crate schedule_recv;

use num::traits::Zero;
use num::Float;
use schedule_recv::periodic_ms;
use std::f64::consts::PI;
use std::ops::Mul;
use std::sync::mpsc::{self, SendError, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

pub type Actor<S> = Sender<Box<Fn(u32) -> S + Send>>;
pub type ActorResult<S> = Result<(), SendError<Box<Fn(u32) -> S + Send>>>;

/// Rust supports both shared-memory and actor models of concurrency, and the `Integrator` utilizes
/// both.  We use an `Actor` to send the `Integrator` new functions, while we use a `Mutex`
/// (shared-memory concurrency) to hold the result of the integration.
///
/// Note that these are not the only options here--there are many, many ways you can deal with
/// concurrent access.  But when in doubt, a plain old `Mutex` is often a good bet.  For example,
/// this might look like a good situation for a `RwLock`--after all, there's no reason for a read
/// in the main task to block writes.  Unfortunately, unless you have significantly more reads than
/// writes (which is certainly not the case here), a `Mutex` will usually outperform a `RwLock`.
pub struct Integrator<S: 'static, T: Send> {
    input: Actor<S>,
    output: Arc<Mutex<T>>,
}

/// In Rust, time durations are strongly typed.  This is usually exactly what you want, but for a
/// problem like this--where the integrated value has unusual (unspecified?) units--it can actually
/// be a bit tricky.  Right now, `Duration`s can only be multiplied or divided by `i32`s, so in
/// order to be able to actually do math with them we say that the type parameter `S` (the result
/// of the function being integrated) must yield `T` (the type of the integrated value) when
/// multiplied by `f64`.  We could possibly replace `f64` with a generic as well, but it would make
/// things a bit more complex.
impl<S, T> Integrator<S, T>
where
    S: Mul<f64, Output = T> + Float + Zero,
    T: 'static + Clone + Send + Float,
{
    pub fn new(frequency: u32) -> Integrator<S, T> {
        // We create a pipe allowing functions to be sent from tx (the sending end) to input (the
        // receiving end).  In order to change the function we are integrating from the task in
        // which the Integrator lives, we simply send the function through tx.
        let (tx, input) = mpsc::channel();
        // The easiest way to do shared-memory concurrency in Rust is to use atomic reference
        // counting, or Arc, around a synchronized type (like Mutex<T>).  Arc gives you a guarantee
        // that memory will not be freed as long as there is at least one reference to it.
        // It is similar to C++'s shared_ptr, but it is guaranteed to be safe and is never
        // incremented unless explicitly cloned (by default, it is moved).
        let s: Arc<Mutex<T>> = Arc::new(Mutex::new(Zero::zero()));
        let integrator = Integrator {
            input: tx,
            // Here is the aforementioned clone.  We have to do it before s enters the closure,
            // because once that happens it is moved into the closure (and later, the new task) and
            // becomes inaccessible to the outside world.
            output: Arc::clone(&s),
        };
        thread::spawn(move || -> () {
            // The frequency is how often we want to "tick" as we update our integrated total.  In
            // Rust, timers can yield Receivers that are periodically notified with an empty
            // message (where the period is the frequency).  This is useful because it lets us wait
            // on either a tick or another type of message (in this case, a request to change the
            // function we are integrating).
            let periodic = periodic_ms(frequency);
            let mut t = 0;
            let mut k: Box<Fn(u32) -> S + Send> = Box::new(|_| Zero::zero());
            let mut k_0: S = Zero::zero();
            loop {
                // Here's the selection we talked about above.  Note that we are careful to call
                // the *non*-failing function, recv(), here.  The reason we do this is because
                // recv() will return Err when the sending end of a channel is dropped.  While
                // this is unlikely to happen for the timer (so again, you could argue for failure
                // there), it's normal behavior for the sending end of input to be dropped, since
                // it just happens when the Integrator falls out of scope.  So we handle it cleanly
                // and break out of the loop, rather than failing.
                select! {
                    res = periodic.recv() => match res {
                        Ok(_) => {
                            t += frequency;
                            let k_1: S = k(t);
                            // Rust Mutexes are a bit different from Mutexes in many other
                            // languages, in that the protected data is actually encapsulated by
                            // the Mutex.  The reason for this is that Rust is actually capable of
                            // enforcing (via its borrow checker) the invariant that the contents
                            // of a Mutex may only be read when you have acquired its lock.  This
                            // is enforced by way of a MutexGuard, the return value of lock(),
                            // which implements some special traits (Deref and DerefMut) that allow
                            // access to the inner element "through" the guard.  The element so
                            // acquired has a lifetime bounded by that of the MutexGuard, the
                            // MutexGuard can only be acquired by taking a lock, and the only way
                            // to release the lock is by letting the MutexGuard fall out of scope,
                            // so it's impossible to access the data incorrectly.  There are some
                            // additional subtleties around the actual implementation, but that's
                            // the basic idea.
                            let mut s = s.lock().unwrap();
                            *s = *s + (k_1 + k_0) * (f64::from(frequency) / 2.);
                            k_0 = k_1;
                        }
                        Err(_) => break,
                    },
                    res = input.recv() => match res {
                        Ok(k_new) => k = k_new,
                        Err(_) => break,
                    }
                }
            }
        });
        integrator
    }

    pub fn input(&self, k: Box<Fn(u32) -> S + Send>) -> ActorResult<S> {
        // The meat of the work is done in the other thread, so to set the
        // input we just send along the Sender we set earlier...
        self.input.send(k)
    }

    pub fn output(&self) -> T {
        // ...and to read the input, we simply acquire a lock on the output Mutex and return a
        // copy. Why do we have to copy it?  Because, as mentioned above, Rust won't let us
        // retain access to the interior of the Mutex unless we have possession of its lock.  There
        // are ways and circumstances in which one can avoid this (e.g. by using atomic types) but
        // a copy is a perfectly reasonable solution as well, and a lot easier to reason about :)
        *self.output.lock().unwrap()
    }
}

/// This function is fairly straightforward.  We create the integrator, set its input function k(t)
/// to 2pi * f * t, and then wait as described in the Rosetta stone problem.
fn integrate() -> f64 {
    let object = Integrator::new(10);
    object
        .input(Box::new(|t: u32| {
            let two_seconds_ms = 2 * 1000;
            let f = 1. / f64::from(two_seconds_ms);
            (2. * PI * f * f64::from(t)).sin()
        }))
        .expect("Failed to set input");
    thread::sleep(Duration::from_secs(2));
    object.input(Box::new(|_| 0.)).expect("Failed to set input");
    thread::sleep(Duration::from_millis(500));
    object.output()
}

fn main() {
    println!("{}", integrate());
}

/// Will fail on a heavily loaded machine
#[test]
#[ignore]
fn solution() {
    // We should just be able to call integrate, but can't represent the closure properly due to
    // rust-lang/rust issue #17060 if we make frequency or period a variable.
    // FIXME(pythonesque): When unboxed closures are fixed, fix integrate() to take two arguments.
    let object = Integrator::new(10);
    object
        .input(Box::new(|t: u32| {
            let two_seconds_ms = 2 * 1000;
            let f = 1. / (two_seconds_ms / 10) as f64;
            (2. * PI * f * t as f64).sin()
        }))
        .expect("Failed to set input");
    thread::sleep(Duration::from_millis(200));
    object.input(Box::new(|_| 0.)).expect("Failed to set input");
    thread::sleep(Duration::from_millis(100));
    assert_eq!(object.output() as u32, 0)
}
```


## Scala



```Scala
object ActiveObject {

  class Integrator {

    import java.util._
    import scala.actors.Actor._

    case class Pulse(t: Double)
    case class Input(k: Double => Double)
    case object Output
    case object Bye

    val timer = new Timer(true)
    var k: Double => Double = (_ => 0.0)
    var s: Double = 0.0
    var t0: Double = 0.0

    val handler = actor {
      loop {
        react {
          case Pulse(t1) => s += (k(t1) + k(t0)) * (t1 - t0) / 2.0; t0 = t1
          case Input(k) => this.k = k
          case Output => reply(s)
          case Bye => timer.cancel; exit
        }
      }
    }

    timer.scheduleAtFixedRate(new TimerTask {
      val start = System.currentTimeMillis
      def run { handler ! Pulse((System.currentTimeMillis - start) / 1000.0) }
    }, 0, 10) // send Pulse every 10 ms

    def input(k: Double => Double) = handler ! Input(k)
    def output = handler !? Output
    def bye = handler ! Bye
  }

  def main(args: Array[String]) {
    val integrator = new Integrator
    integrator.input(t => Math.sin(2.0 * Math.Pi * 0.5 * t))
    Thread.sleep(2000)
    integrator.input(_ => 0.0)
    Thread.sleep(500)
    println(integrator.output)
    integrator.bye
  }
}
```



## Smalltalk



```Smalltalk

Object subclass:#Integrator
        instanceVariableNames:'tickRate input s thread'
        classVariableNames:''
        poolDictionaries:''
        category:'Rosetta'

instance methods:

input:aFunctionOfT
    input := aFunctionOfT.

startWithTickRate:r
    "setup and start sampling"
    tickRate := r.
    s := 0.
    thread := [ self integrateLoop ] fork.

stop
    "stop and return the 'final' output"
    thread terminate.
    ^ s

integrateLoop
    "no need for any locks
     - the assignment to s is atomic in Smallalk; its either done or not, when terminated, so who cares"

    |tBegin tPrev tNow kPrev kNow deltaT delta|

    tBegin := tPrev := Timestamp nowWithMilliseconds.
    kPrev := input value:0.

    [true] whileTrue:[
        Delay waitForSeconds: tickRate.
        tNow := Timestamp nowWithMilliseconds.
        kNow := input value:(tNow millisecondDeltaFrom:tBegin) / 1000.

        deltaT := (tNow millisecondDeltaFrom:tPrev) / 1000.
        delta := (kPrev + kNow) * deltaT / 2.

        s := s + delta.
        tPrev := tNow. kPrev := kNow.
    ].

class methods:

example
    #( 0.5 0.1 0.05 0.01 0.005 0.001 0.0005 ) do:[:sampleRate |
        |i|

        i := Integrator new.
        i input:[:t | (2 * Float pi * 0.5 * t) sin].
        i startWithTickRate:sampleRate.

        Delay waitForSeconds:2.
        i input:[:t | 0].
        Delay waitForSeconds:0.5.

        Transcript
            show:'Sample rate: '; showCR:sampleRate;
            showCR:(i stop).
    ].

```

running:

```Smalltalk>Integrator example</lang


output:

 Sample rate: 0.5
 -0.0258202058271805
 Sample rate: 0.1
 -0.00519217893508676
 Sample rate: 0.05
 -0.000897807957672559
 Sample rate: 0.01
 -0.000650159409949159
 Sample rate: 0.005
 -0.00033633922519125
 Sample rate: 0.001
 0.000286557714782226
 Sample rate: 0.0005
 0.000253571129723327

for backward compatibility, the smalltalk used here returns only timestamps with second-precision from "Timestamp now". Therefore, the millisecond-precision variant was used here. An alternative would have been to ask the OS for its ticker, which is more precise.


## SuperCollider

Instead of writing a class, here we just use an environment to encapsulate state.

```SuperCollider

(
a = TaskProxy { |envir|
	envir.use {
		~integral = 0;
		~time = 0;
		~prev = 0;
		~running = true;
		loop {
			~val = ~input.(~time);
			~integral = ~integral + (~val + ~prev * ~dt / 2);
			~prev = ~val;
			~time = ~time + ~dt;
			~dt.wait;
		}
	}
};
)

// run the test
(
fork {
	a.set(\dt, 0.0001);
	a.set(\input, { |t| sin(2pi * 0.5 * t) });
	a.play(quant: 0); // play immediately
	2.wait;
	a.set(\input, 0);
	0.5.wait;
	a.stop;
	a.get(\integral).postln; // answers -7.0263424372343e-15
}
)

```



## Swift


```Swift
// For NSObject, NSTimeInterval and NSThread
import Foundation
// For PI and sin
import Darwin

class ActiveObject:NSObject {

    let sampling = 0.1
    var K: (t: NSTimeInterval) -> Double
    var S: Double
    var t0, t1: NSTimeInterval
    var thread = NSThread()

    func integrateK() {
        t0 = t1
        t1 += sampling
        S += (K(t:t1) + K(t: t0)) * (t1 - t0) / 2
    }

    func updateObject() {
        while true {
            integrateK()
            usleep(100000)
        }
    }

    init(function: (NSTimeInterval) -> Double) {
        S = 0
        t0 = 0
        t1 = 0
        K = function
        super.init()
        thread = NSThread(target: self, selector: "updateObject", object: nil)
        thread.start()
    }

    func Input(function: (NSTimeInterval) -> Double) {
        K = function

    }

    func Output() -> Double {
        return S
    }

}

// main
func sine(t: NSTimeInterval) -> Double {
    let f = 0.5

    return sin(2 * M_PI * f * t)
}

var activeObject = ActiveObject(function: sine)

var date = NSDate()

sleep(2)

activeObject.Input({(t: NSTimeInterval) -> Double in return 0.0})

usleep(500000)

println(activeObject.Output())

```

Sample output:

```txt

1.35308431126191e-16

```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

This implementation Tcl 8.6 for object support (for the active integrator object) and coroutine support (for the controller task). It could be rewritten to only use 8.5 plus the TclOO library.

```Tcl
package require Tcl 8.6
oo::class create integrator {
    variable e sum delay tBase t0 k0 aid
    constructor {{interval 1}} {
	set delay $interval
	set tBase [clock microseconds]
	set t0 0
	set e { 0.0 }
	set k0 0.0
	set sum 0.0
	set aid [after $delay [namespace code {my Step}]]
    }
    destructor {
	after cancel $aid
    }
    method input expression {
	set e $expression
    }
    method output {} {
	return $sum
    }
    method Eval t {
	expr $e
    }
    method Step {} {
	set aid [after $delay [namespace code {my Step}]]
	set t [expr {([clock microseconds] - $tBase) / 1e6}]
	set k1 [my Eval $t]
	set sum [expr {$sum + ($k1 + $k0) * ($t - $t0) / 2.}]
	set t0 $t
	set k0 $k1
    }
}

set pi 3.14159265
proc pause {time} {
    yield [after [expr {int($time * 1000)}] [info coroutine]]
}
proc task {script} {
    coroutine task_ apply [list {} "$script;set ::done ok"]
    vwait done
}
task {
    integrator create i
    i input {sin(2*$::pi * 0.5 * $t)}
    pause 2
    i input { 0.0 }
    pause 0.5
    puts [format %.15f [i output]]
}
```

Sample output:
 -0.000000168952702


## Visual Basic .NET


Since this object is CPU intensive, shutting it down when done is crucial. To facilitate this, the IDisposable pattern was used.


```vbnet
Module Module1

    Sub Main()
        Using active As New Integrator
            active.Operation = Function(t As Double) Math.Sin(2 * Math.PI * 0.5 * t)
            Threading.Thread.Sleep(TimeSpan.FromSeconds(2))
            Console.WriteLine(active.Value)
            active.Operation = Function(t As Double) 0
            Threading.Thread.Sleep(TimeSpan.FromSeconds(0.5))
            Console.WriteLine(active.Value)
        End Using
        Console.ReadLine()
    End Sub

End Module

Class Integrator
    Implements IDisposable

    Private m_Operation As Func(Of Double, Double)
    Private m_Disposed As Boolean
    Private m_SyncRoot As New Object
    Private m_Value As Double

    Public Sub New()
        m_Operation = Function(void) 0.0
        Dim t As New Threading.Thread(AddressOf MainLoop)
        t.Start()
    End Sub

    Private Sub MainLoop()
        Dim epoch = Now
        Dim t0 = 0.0
        Do
            SyncLock m_SyncRoot
                Dim t1 = (Now - epoch).TotalSeconds
                m_Value = m_Value + (Operation(t1) + Operation(t0)) * (t1 - t0) / 2
                t0 = t1
            End SyncLock
            Threading.Thread.Sleep(10)
        Loop Until m_Disposed
    End Sub

    Public Property Operation() As Func(Of Double, Double)
        Get
            SyncLock m_SyncRoot
                Return m_Operation
            End SyncLock
        End Get
        Set(ByVal value As Func(Of Double, Double))
            SyncLock m_SyncRoot
                m_Operation = value
            End SyncLock
        End Set
    End Property

    Public ReadOnly Property Value() As Double
        Get
            SyncLock m_SyncRoot
                Return m_Value
            End SyncLock
        End Get
    End Property

    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        m_Disposed = True
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub

End Class
```



 Output: 0.000241446762282308


## zkl

{{trans|Python}}
Uses cheese ball thread safety: since the integrator runs continuously and I don't want to queue the output, just sample it, strong references are used as they change atomically.

```zkl
class Integrator{
   // continuously integrate a function `K`, at each `interval` seconds'
   fcn init(f,interval=1e-4){
      var _interval=interval, K=Ref(f), S=Ref(0.0), run=True;
      self.launch();  // start me as a thread
   }
   fcn liftoff{ // entry point for the thread
      start:=Time.Clock.timef;  // floating point seconds since Epoch
      t0,k0,s:=0,K.value(0),S.value;
      while(run){
	 Atomic.sleep(_interval);
	 t1,k1:=Time.Clock.timef - start, K.value(t1);
	 s+=(k1 + k0)*(t1 - t0)/2.0; S.set(s);
	 t0,k0=t1,k1;
      }
   }
   fcn sample  { S.value  }
   fcn setF(f) { K.set(f) }
}
```


```zkl
ai:=Integrator(fcn(t){ ((0.0).pi*t).sin() });
Atomic.sleep(2);
ai.sample().println();

ai.setF(fcn{ 0 });
Atomic.sleep(0.5);
ai.sample().println();
```

{{out}}

```txt

4.35857e-09
1.11571e-07

```


{{omit from|ACL2}}
{{omit from|AWK}}
{{omit from|gnuplot}}
{{omit from|GUISS}}
{{omit from|LaTeX}}
{{omit from|Locomotive Basic}}
{{omit from|Make}}
{{omit from|Metafont}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|Octave}}
{{omit from|PlainTeX}}
{{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
{{omit from|Retro}}
{{omit from|UNIX Shell}}
{{omit from|ZX Spectrum Basic}}
