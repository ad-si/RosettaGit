+++
title = "Events"
description = ""
date = 2019-08-19T16:30:06Z
aliases = []
[extra]
id = 4361
[taxonomies]
categories = []
tags = []
+++

{{task|Concurrency}}{{requires|Concurrency}}[[Category:Encyclopedia]]
'''Event''' is a synchronization object. An event has two states ''signaled'' and ''reset''. A [[task]] may await for the event to enter the desired state, usually the ''signaled'' state. It is released once the state is entered. Releasing waiting tasks is called ''event notification''. Programmatically controlled events can be set by a [[task]] into one of its states.

In [[concurrent programming]] event also refers to a notification that some state has been reached through an asynchronous activity. The source of the event can be:

* ''internal'', from another [[task]], programmatically;
* ''external'', from the hardware, such as user input, timer, etc. Signaling an event from the hardware is accomplished by means of hardware [[interrupts]].

Event is a low-level synchronization mechanism. It neither identify the state that caused it signaled, nor the source of, nor who is the subject of notification. Events augmented by data and/or publisher-subscriber schemes are often referred as '''messages''', '''signals''' etc.

In the context of general programming '''event-driven architecture''' refers to a design that deploy events in order to synchronize [[task]]s with the asynchronous activities they must be aware of. The opposite approach is '''polling''' sometimes called '''busy waiting''', when the synchronization is achieved by an explicit periodic querying the state of the activity. As the name suggests busy waiting consumes system resources even when the external activity does not change its state.

Event-driven architectures are widely used in GUI design and SCADA systems. They are flexible and have relatively short response times. At the same time event-driven architectures suffer to the problems related to their unpredictability. They face [[race condition]], deadlocking, live locks and priority inversion. For this reason [[real-time computing|real-time]] systems tend to polling schemes, trading performance for predictability in the worst case scenario.

=Variants of events=

==Manual-reset event==
This event changes its state by an explicit request of a [[task]]. I.e. once signaled it remains in this state until it will be explicitly reset.

==Pulse event==
A pulse event when signaled releases all [[task]]s awaiting it and then is automatically reset.

=Sample implementations / APIs=
Show how a manual-reset event can be implemented in the language or else use an API to a library that provides events. Write a program that waits 1s and then signals the event to a [[task]] waiting for the event.


## Ada

[[Ada]] provides higher-level concurrency primitives, which are complete in the sense that they also allow implementations of the lower-level ones, like event. Here is an implementation of the manual-reset event.

The event interface:

```ada
protected type Event is
   procedure Signal;
   procedure Reset;
   entry Wait;
private
   Fired : Boolean := False;
end Event;
```

The event implementation:

```ada
protected body Event is
   procedure Signal is
   begin
      Fired := True;
   end Signal;
   procedure Reset is
   begin
      Fired := False;
   end Reset;
   entry Wait when Fired is
   begin
      null;
   end Wait;
end Event;
```

With the event defined above:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Events is
   -- Place the event implementation here
   X : Event;

   task A;
   task body A is
   begin
      Put_Line ("A is waiting for X");
      X.Wait;
      Put_Line ("A received X");
   end A;
begin
   delay 1.0;
   Put_Line ("Signal X");
   X.Signal;
end Test_Events;
```

Sample output:

```txt

A is waiting for X
Signal X
A received X

```



## AutoHotkey


```AutoHotkey
SetTimer, internal, 1000
Return

internal:  ; fire on a timer
  TrayTip, internal, internal event!`npress F2 for external event
  SetTimer, internal, off
Return

F2::   ; external event: fire on F2 key press
  TrayTip, external, f2 key pressed
Return
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

### API

This uses a Windows event object:

```bbcbasic
      INSTALL @lib$+"TIMERLIB"
      WAIT_TIMEOUT = 258

      SYS "CreateEvent", 0, 1, 0, 0 TO hEvent%

      timerID% = FN_ontimer(1000, PROCelapsed, 0)

      PRINT "Waiting for event..."
      REPEAT
        SYS "WaitForSingleObject", hEvent%, 1 TO res%
      UNTIL res% <> WAIT_TIMEOUT
      PRINT "Event signalled"
      END

      DEF PROCelapsed
      SYS "SetEvent", hEvent%
      ENDPROC
```


### Native

This uses a simple variable as a semaphore:

```bbcbasic
      INSTALL @lib$+"TIMERLIB"

      Event% = FALSE

      timerID% = FN_ontimer(1000, PROCelapsed, 0)

      PRINT "Waiting for event..."
      REPEAT
        WAIT 0
      UNTIL Event%
      PRINT "Event signalled"
      END

      DEF PROCelapsed
      Event% = TRUE
      ENDPROC
```



## C

Using pipe to communicate to <code>fork</code>ed child.  Since child will be blocking trying to read the other end of the pipe, this can be used for synchronization.

```c
#include <stdio.h>
#include <unistd.h>

int main()
{
	int p[2];
	pipe(p);
	if (fork()) {
		close(p[0]);
		sleep(1);
		write(p[1], p, 1);
		wait(0);
	} else {
		close(p[1]);
		read(p[0], p + 1, 1);
		puts("received signal from pipe");
	}
	return 0;
}
```


## C#

```c#
using System;
using System.Timers;

class Program
{
    static void Main()
    {
        var timer = new Timer(1000);
        timer.Elapsed += new ElapsedEventHandler(OnElapsed);
        Console.WriteLine(DateTime.Now);
        timer.Start();
        Console.ReadLine();
    }

    static void OnElapsed(object sender, ElapsedEventArgs eventArgs)
    {
        Console.WriteLine(eventArgs.SignalTime);
        ((Timer)sender).Stop();
    }
}
```

Sample output:

```txt
10-11-2010 18:35:11
10-11-2010 18:35:12
```



## Clojure

{{trans|Go}}

```lisp
(ns async-example.core
  (:require [clojure.core.async :refer [>! <! >!! <!! go chan]])
  (:require [clj-time.core :as time])
  (:require [clj-time.format :as time-format])
  (:gen-class))

;; Helper functions (logging & time stamp)
; Time stamp format
(def custom-formatter (time-format/formatter "yyyy:MM:dd:ss.SS"))

(defn safe-println [& more]
  " This function avoids interleaving of text output when using println due to race condition for multi-processes printing
    as discussed http://yellerapp.com/posts/2014-12-11-14-race-condition-in-clojure-println.html "
  (.write *out* (str (clojure.string/join " " more) "\n")))

(defn log [s]
  " Outputs mesage with time stamp "
  (safe-println (time-format/unparse custom-formatter (time/now)) ":" s))

;; Main code
(defn -main [& args]
  (let [c (chan)]
    (log "Program start")
    (go
      (log "Task start")
      (log (str "Event received by task: "(<! c))))

    (<!!
      (go
        (log "program sleeping")
        (Thread/sleep 1000)     ; Wait 1 second
        (log "Program signaling event")
        (>! c "reset")          ; Send message to task
          ))))

; Invoke -main function
(-main)

```

{{Output}}

```txt

2016:10:18:06.93 : Program start
2016:10:18:06.94 : task start
2016:10:18:06.94 : program sleeping
2016:10:18:07.94 : Program signaling event
2016:10:18:07.94 : Event received by task: reset

```


## Delphi


```Delphi
program Events;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows;

type
  TWaitThread = class(TThread)
  private
    FEvent: THandle;
  public
    procedure Sync;
    procedure Execute; override;
    constructor Create(const aEvent: THandle); reintroduce;
  end;

{ TWaitThread }

constructor TWaitThread.Create(const aEvent: THandle);
begin
  inherited Create(False);
  FEvent := aEvent;
end;

procedure TWaitThread.Execute;
var
  res: Cardinal;
begin
  res := WaitForSingleObject(FEvent, INFINITE);
  if res = 0 then
    Synchronize(Sync);
end;

procedure TWaitThread.Sync;
begin
  Writeln(DateTimeToStr(Now));
end;

var
  event: THandle;

begin
  Writeln(DateTimeToStr(Now));
  event := CreateEvent(nil, False, False, 'Event');
  with TWaitThread.Create(event) do
  try
    Sleep(1000);
    SetEvent(event)
  finally
    Free;
  end;
  Readln;
end.
```

Sample output:

```txt

09.08.2011 0:27:43
09.08.2011 0:27:44

```



## E


```e
def makeEvent() {
    def [var fired, var firer] := Ref.promise()

    def event {
        to signal() {
            firer.resolveRace(null) # all current and future wait()s will resolve
        }
        to reset() {
            if (firer.isDone()) { # ignore multiple resets. If we didn't, then
                                  # reset() wait() reset() signal() would never
                                  # resolve that wait().
                # create all fresh state
                def [p, r] := Ref.promise()
                fired := p
                firer := r
            }
        }
        to wait() {
            return fired
        }
    }

    return event
}
```

The event object has this behavior: the return value of <code>.wait()</code> will be resolved after the time of the earliest <code>.signal()</code> for which there is no intervening <code>.reset()</code>.

```e
def e := makeEvent()

{
    when (e.wait()) -> {
        println("[2] Received event.")
    }
    println("[2] Waiting for event...")
}

{
    timer.whenPast(timer.now() + 1000, def _() {
        println("[1] Signaling event.")
        e.signal()
    })
    println("[1] Waiting 1 second...")
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Events do
  def log(msg) do
    time = Time.utc_now |> to_string |> String.slice(0..7)
    IO.puts "#{time} => #{msg}"
  end

  def task do
    log("Task start")
    receive do
      :go -> :ok
    end
    log("Task resumed")
  end

  def main do
    log("Program start")
    {pid,ref} = spawn_monitor(__MODULE__,:task,[])
    log("Program sleeping")
    Process.sleep(1000)
    log("Program signalling event")
    send(pid, :go)
    receive do
      {:DOWN,^ref,_,_,_} -> :task_is_down
    end
  end
end

Events.main
```


{{out}}

```txt

06:27:05 => Program start
06:27:05 => Program sleeping
06:27:05 => Task start
06:27:06 => Program signalling event
06:27:06 => Task resumed

```



## Erlang

Events can be implemented by using the selective receive expression and erlang's built in message passing. Here task waits for the message 'go' before it will continue.

```erlang

-module(events).
-compile(export_all).

log(Msg) ->
    {H,M,S} = erlang:time(),
    io:fwrite("~2.B:~2.B:~2.B => ~s~n",[H,M,S,Msg]).

task() ->
    log("Task start"),
    receive
        go -> ok
    end,
    log("Task resumed").

main() ->
    log("Program start"),
    P = spawn(?MODULE,task,[]),
    log("Program sleeping"),
    timer:sleep(1000),
    log("Program signalling event"),
    P ! go,
    timer:sleep(100).

```

'''Output:'''

```erlang

66> events:main().
 0: 0:57 => Program start
 0: 0:57 => Program sleeping
 0: 0:57 => Task start
 0: 0:58 => Program signalling event
 0: 0:58 => Task resumed
ok

```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System
open System.Timers

let onElapsed (sender : obj) (eventArgs : ElapsedEventArgs) =
    printfn "%A" eventArgs.SignalTime
    (sender :?> Timer).Stop()

[<EntryPoint>]
let main argv =
    let timer = new Timer(1000.)
    timer.Elapsed.AddHandler(new ElapsedEventHandler(onElapsed))
    printfn "%A" DateTime.Now
    timer.Start()
    ignore <| Console.ReadLine()
    0
```



## Gambas


```gambas
Public Sub Timer1_Timer()

Print Str(Time(Now))

End
```

Output:

```txt

16:14:18
16:14:19
16:14:20
16:14:21
16:14:22
16:14:23
16:14:24
16:14:25

```



## Go

A Go channel can represent an manual-reset event, as described by the task.  The two states of signaled and reset correspond to the presence or absence of a value on the channel.  The program signals by sending a value on the channel.  The event is reset when the waiting task explicitly executes the channel receive operation, <-event.

```go
package main

import (
    "log"
    "os"
    "time"
)

func main() {
    l := log.New(os.Stdout, "", log.Ltime | log.Lmicroseconds)
    l.Println("program start")
    event := make(chan int)
    go func() {
        l.Println("task start")
        <-event
        l.Println("event reset by task")
    }()
    l.Println("program sleeping")
    time.Sleep(1 * time.Second)
    l.Println("program signaling event")
    event <- 0
    time.Sleep(100 * time.Millisecond)
}
```

{{out}}

```txt

01:27:21.862000 program start
01:27:21.862245 program sleeping
01:27:21.867269 task start
01:27:22.868294 program signaling event
01:27:22.868346 event reset by task

```



## Haskell


```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.SampleVar

-- An Event is defined as a SampleVar with no data.
-- http://haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-SampleVar.html
newtype Event = Event (SampleVar ())

newEvent               = fmap Event (newEmptySampleVar)
signalEvent (Event sv) = writeSampleVar sv ()
resetEvent  (Event sv) = emptySampleVar sv
waitEvent   (Event sv) = readSampleVar  sv
```


```haskell
main = do e <- newEvent
          forkIO (waitTask e)
          putStrLn "[1] Waiting 1 second..."
          threadDelay 1000000 {- µs -}
          putStrLn "[1] Signaling event."
          signalEvent e
          threadDelay 1000000 {- µs -}    -- defer program exit for reception

waitTask e = do putStrLn "[2] Waiting for event..."
                waitEvent e
                putStrLn "[2] Received event."
```

Note: Because there is no serialization of the text output, there is a chance that it will appear interleaved.

==Icon and {{header|Unicon}}==

The following only works in Unicon.  The example illustrates the multiple tasks can
receive the same event:

```unicon
record Event(cond, value)

procedure main()
    event := Event(condvar())
    t1 := thread {
        write("Task one waiting for event....")
        critical event.cond: while /(event.value) do wait(event.cond)
        write("Task one received event.")
        }
    t2 := thread {
        write("Task two waiting for event....")
        critical event.cond: while /(event.value) do wait(event.cond)
        write("Task two received event.")
        }
    delay(1000)                   # Let main thread post the event.
    event.value := "yes"
    write("Signalling event.")
    signal(event.cond,0)
    every wait(t1|t2)
end
```


Sample run:

```txt

->event
Task two waiting for event....
Task one waiting for event....
Signalling event.
Task two received event.
Task one received event.
->

```



## Julia

Julia provides a variety of high and low level functions and macros for multitasking and events.
The code below uses a Condition() event semaphore created in the base thread for communication
between two child threads.


```julia

function dolongcomputation(cond)
    det(rand(4000, 4000))
    Base.notify(cond)
end

function printnotice(cond)
    Base.wait(cond)
    println("They are finished.")
end

function delegate()
    println("Starting task, sleeping...")
    condition = Base.Condition()
    Base.@async(printnotice(condition))
    Base.@async(dolongcomputation(condition))
end

delegate()
sleep(5)
println("Done sleeping.")

```

{{output}}
```txt

Starting task, sleeping...
They are finished.
Done sleeping.

```



## LFE


{{trans|Erlang}}

Paste in the REPL:


```lisp

(defun log (msg)
  (let ((`#(,h ,m ,s) (erlang:time)))
    (lfe_io:format "~2.B:~2.B:~2.B => ~s~n" `(,h ,m ,s ,msg))))

(defun task ()
  (log "Task start")
  (receive
    ('go 'ok))
  (log "Task resumed"))

(defun run ()
  (log "Program start")
  (let ((pid (spawn (lambda () (task)))))
    (progn
      (log "Program sleeping")
      (timer:sleep 1000)
      (log "Program signalling event")
      (! pid 'go)
      (timer:sleep 100))))

```


Usage:


```txt

> (run)
18:34:53 => Program start
18:34:53 => Program sleeping
18:34:53 => Task start
18:34:54 => Program signalling event
18:34:54 => Task resumed
ok

```

OTP comes with a <code>gen_even</code>t behavior that is more robust and resilient than this version. That is what should be used for any non-toy example or project.


## Lingo

Lingo/Director uses (stateless) events for system/application state change notifications, user action notifications and inter-sprite communication.

To catch an event, a corresponding event handler - a function with a predefined name - has to be definined in the code. Examples for such event handlers are:

```lingo
-- the current window was closed
on closeWindow
...
end

-- the left mouse button was pressed by the user
on mouseDown
...
end
```

Also "Sprites" (visual elements) receive events by setting up such event handlers in scripts attached to them. Both predefined and custom events can be sent programmatically to sprites, e.g. using:

```lingo
-- send event #mouseDown programmatically to sprite 1
sendSprite(1, #mouseDown)

-- send custom event #foo to named sprite "bar"
sendSprite("bar", #foo)

-- send custom event #fooBar to all existing sprites
sendAllSprites(#fooBar)
```


Using a binary plugin ("Xtra"), in Windows also lower level window messages can be both sent and received:
{{libheader|Msg Xtra}}

```lingo
mx = xtra("Msg").new()

-- send message WM_LBUTTONDOWN to a specific window identified by HWND hwnd
WM_LBUTTONDOWN = 513
MK_LBUTTON = 1
lParam = 65536*y + x
mx.send_msg (hwnd, WM_LBUTTONDOWN, MK_LBUTTON, lParam)

-- listen for WM_COPYDATA and WM_MOUSEWHEEL messages sent to current application
-- window, notify Lingo callback function 'msgReceived' when such messages occur.
-- This callback function will receive hwnd, message, wParam and lParam as arguments
-- (and for WM_COPYDATA messages also the data that was sent as ByteArray).
WM_COPYDATA = 74
WM_MOUSEWHEEL = 522
mx.msg_listen([WM_COPYDATA, WM_MOUSEWHEEL], VOID, #msgReceived)
```



## JavaScript

An example using the [[wp:Yahoo!_UI_Library|YUI]] library:

```javascript
YUI().use('event-custom', function(Y) {
    // add a custom event:
    Y.on('my:event', function () {
        alert("Event fired");
    });
    // fire the event after one second:
    setTimeout(function () {
        Y.fire('my:event');
    }, 1000);
});
```

An example simulating [[wp:Document_Object_Model|DOM]] events:

```javascript
YUI().use('node-event-simulate', function(Y) {
    // add a click event handler to a DOM node with id "button":
    Y.one("#button").on("click", function (e) {
        alert("Button clicked");
    });
    // simulate the click after one second:
    setTimeout(function () {
        Y.one("#button").simulate("click");
    }, 1000);
});
```



## Mathematica

Mathematica supports events from timers (via Pause[]), task schedule descriptors. This will print a message after 4 seconds, then terminate the program.

```Mathematica
Print["Will exit in 4 seconds"]; Pause[4]; Quit[]
->Will exit in 4 seconds
```



## Nim

{{trans|C}}

```nim
import posix

var p: array[2, cint]
discard pipe p
if fork() > 0:
  discard close p[0]
  discard sleep 1
  discard p[1].write(addr p[0], 1)
  var x: cint = 0
  discard wait x
else:
  discard close p[1]
  discard p[0].read(addr p[1], 1)
  echo "received signal from pipe"
```




## Oforth


An event is often implemented with a control channel. A task is waiting for an object on the channel. When the event occurs, another task sends an object on this channel.


```Oforth
: anEvent
| ch |
   Channel new ->ch
   #[ ch receive "Ok, event is signaled !" println ] &
   System sleep(1000)
   ch send($myEvent) ;
```


An emitter is a general implementation for handling events : an emitter waits for events emitted and launches listeners that are waiting for those events.

```Oforth
import: emitter

: anEvent2
| e i |
   Emitter new(null) ->e
   e onEvent($myEvent, #[ "Event is signaled !" println ])
   10 loop: i [
      1000 System sleep
      $myEvent e emit
      ]
   e close ;
```



## Oz

{{trans|Haskell}}
Events can be implemented as mutable references to dataflow variables:

```oz
declare
  fun {NewEvent}
     {NewCell _}
  end

  proc {SignalEvent Event}
     @Event = unit
  end

  proc {ResetEvent Event}
     Event := _
  end

  proc {WaitEvent Event}
     {Wait @Event}
  end

  E = {NewEvent}
in
  thread
     {System.showInfo "[2] Waiting for event..."}
     {WaitEvent E}
     {System.showInfo "[2] Received event."}
  end

  {System.showInfo "[1] Waiting 1 second..."}
  {Delay 1000}
  {System.showInfo "[1] Signaling event."}
  {SignalEvent E}
```

However, this code is quite unidiomatic. If we need to wait for an event just once (like in this example), we can simply use a dataflow variable, i.e. an event that cannot be reset:

```oz
declare
  E
in
  thread
     {System.showInfo "[2] Waiting for event..."}
     {Wait E}
     {System.showInfo "[2] Received event."}
  end

  {System.showInfo "[1] Waiting 1 second..."}
  {Delay 1000}
  {System.showInfo "[1] Signaling event."}
  E = unit
```

If we want to synchronize two threads repeatedly and exchange data, it is natural to use ports and streams. Streams are just lists with an unbound tail. A port is basically a pointer to the tail of a list, i.e. it keeps track of where the next event can be written to:

```oz
declare
  MyPort
in
  thread
     MyStream
  in
     {NewPort ?MyStream ?MyPort}
     {System.showInfo "[2] Waiting for event..."}
     for Event in MyStream do
	{System.showInfo "[2] Received event."}
	{System.showInfo "[2] Waiting for event again..."}
     end
  end

  for do
     {System.showInfo "[1] Waiting 1 second..."}
     {Delay 1000}
     {System.showInfo "[1] Signaling event."}
     {Port.send MyPort unit}
  end
```

It is important to limit the scope of a stream as much as possible to ensure that the already read part of the stream is garbage-collected.


## Perl

This is an example of using the [http://search.cpan.org/perldoc?AnyEvent AnyEvent] module.
The result is this: it prints "Hello world!" after one second, then after another second prints "Hi!" four times every quarter of a second and then immediately prints "Bye!" and quits:

```Perl
use AnyEvent;

# a new condition with a callback:
my $quit = AnyEvent->condvar(
    cb => sub {
        warn "Bye!\n";
    }
);

# a new timer, starts after 2s and repeats every 0.25s:
my $counter = 1;
my $hi = AnyEvent->timer(
    after => 2,
    interval => 0.25,
    cb => sub {
        warn "Hi!\n";
        # flag the condition as ready after 4 times:
        $quit->send if ++$counter > 4;
    }
);

# another timer, runs the callback once after 1s:
my $hello = AnyEvent->timer(
    after => 1,
    cb => sub {
        warn "Hello world!\n";
    }
);

# wait for the $quit condition to be ready:
$quit->recv();
```

This is the same using AnyEvent [http://search.cpan.org/perldoc?AE simplified API]:

```Perl
use AnyEvent;

my $quit = AE::cv sub { warn "Bye!\n" };

my $counter = 1;
my $hi = AE::timer 2, 0.25, sub {
    warn "Hi!\n";
    $quit->send if ++$counter > 4;
};

my $hello = AE::timer 1, 0, sub {
    warn "Hello world!\n";
};

$quit->recv;
```



## Perl 6

{{trans|Go}}

```perl6
note now, " program start";
my $event = Channel.new;

my $todo = start {
    note now, " task start";
    $event.receive;
    note now, " event reset by task";
}

note now, " program sleeping";
sleep 1;
note now, " program signaling event";
$event.send(0);
await $todo;
```

{{out}}

```txt
Instant:1403880984.089974 program start
Instant:1403880984.095400 program sleeping
Instant:1403880984.095491 task start
Instant:1403880985.099381 program signaling event
Instant:1403880985.109395 event reset by task
```


See also [[Handle_a_signal#Perl_6]] for an example of using Supplies to do reactive programming based on events (Unix signals in this case).


## Phix

The primary synchronisation primitive in phix is the critical section, in the following the leave_cs()
in main() acts as signalling an event, and the one in echo() from whichever goes first acts to signal
that the other can/should resume.

```Phix
constant lock = init_cs()
include timedate.e

procedure showtime()
    puts(1,format_timedate(date()," h:m:s\n"))
end procedure

procedure echo(string s)
    sleep(rnd()/10) -- see note
    enter_cs(lock)
    puts(1,s)
    sleep(1)
    showtime()
    leave_cs(lock)
end procedure

procedure main()
    enter_cs(lock)
    sequence threads = {create_thread(routine_id("echo"),{"job1"}),
                        create_thread(routine_id("echo"),{"job2"})}
    puts(1,"main")
    showtime()
    sleep(1)
    puts(1,"free")
    showtime()
    leave_cs(lock)
    wait_thread(threads)
    puts(1,"done\n")
end procedure
main()
```

{{out}}
Typically the first thread to attempt enter_cs() is released first, but there is
no guarantee of that. The sleep(rnd()/10) above evens out the likelihood, by
pausing for up to 0.1s, but otherwise isn't necessary.

```txt

main 10:00:57
free 10:00:58
job2 10:00:59
job1 10:01:00
done

```

External events such as timers and user input are handled in pGUI, eg

```Phix
function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

Ihandle timer = IupTimer(Icallback("timer_cb"), 1000)

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_F5 then
        iteration = 0
        IupSetInt(timer,"RUN",1)                -- (restart timer)
    end if
    return IUP_CONTINUE
end function

IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
```



## PicoLisp

PicoLisp supports events from timers (via
'[http://software-lab.de/doc/refT.html#task task]' and
'[http://software-lab.de/doc/refA.html#alarm alarm]'),
file descriptors (also 'task') and various
'[http://software-lab.de/doc/refS.html#*Sig1 signals]'.
This will print a message after one second, then terminate the program after
another four seconds:

```PicoLisp
(alarm 1
   (prinl "Exit in 4 seconds")
   (alarm 4 (bye)) )
```



## PowerShell


```PowerShell

$timer = New-Object -TypeName System.Timers.Timer -Property @{Enabled=$true; Interval=1000; AutoReset=$true}

$action = {
    $global:counter += 1
    Write-Host “Event counter is ${counter}: $((Get-Date).ToString("hh:mm:ss"))”
    if ($counter -ge $event.MessageData)
    {
        Write-Host “Timer stopped”
        $timer.Stop()
    }
}

$job = Register-ObjectEvent -InputObject $timer -MessageData 5 -SourceIdentifier Count -EventName Elapsed -Action $action

$global:counter = 0
& $job.Module {$global:counter}

```

{{Out}}

```txt

Event counter is 1: 04:58:04
Event counter is 2: 04:58:05
Event counter is 3: 04:58:06
Event counter is 4: 04:58:07
Event counter is 5: 04:58:08
Timer stopped

```



## PureBasic


```Purebasic
OpenWindow (0, 10, 10, 150, 40, "Event Demo")
ButtonGadget (1, 10, 10, 35, 20, "Quit")

Repeat

   Event = WaitWindowEvent()

   If  Event = #PB_Event_Gadget And EventGadget() = 1
      End
   EndIf

ForEver
```



## Python



```Python

import threading
import time


def wait_for_event(event):
    event.wait()
    print("Thread: Got event")

e = threading.Event()

t = threading.Thread(target=wait_for_event, args=(e,))
t.start()

print("Main: Waiting one second")
time.sleep(1.0)
print("Main: Setting event")
e.set()
time.sleep(1.0)
print("Main: Done")
t.join()

```



## Racket


Racket comes with events as part of its implementation; various types of
events are used for different purposes: there are events that become
ready when some input is available in a port, when a TCP connection is
made, when a thread is dead, etc etc.  Here we use a simple alarm event
as requested, even though it's a odd to send the actual event result to
the task (since it's a useless value):


```racket

#lang racket

(define task (thread (lambda () (printf "Got: ~s\n" (thread-receive)))))

(thread-send task ; wait for it, then send it
             (sync (alarm-evt (+ 1000 (current-inexact-milliseconds)))))

(void (sync task)) ; wait for the task to be done before exiting

```



## REXX

Although REXX can be event driven, most events would probably have to be actively checked to see if the event occurs.

Here is a   ''time-driven''   example of events happening, based on specific timer ticks.

```rexx
/*REXX program demonstrates a  method  of  handling events  (this is a time─driven pgm).*/
signal on halt                                   /*allow the user to  HALT  the program.*/
parse arg timeEvent                              /*allow the  "event"  to be specified. */
if timeEvent=''  then timeEvent=5                /*Not specified?  Then use the default.*/

event?:  do forever                              /*determine if an event has occurred.  */
         theEvent=right(time(),1)                /*maybe it's an event, ─or─  maybe not.*/
         if pos(theEvent,timeEvent)\==0  then  signal happening
         end   /*forever*/

say 'Control should never get here!'             /*This is a logic  can─never─happen !  */
halt: say '════════════ program halted.';  exit  /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
happening: say 'an event occurred at'  time()", the event is:"   theEvent
             do  while theEvent==right(time(),1)
             nop                                 /*replace NOP  with the "process" code.*/
             end   /*while*/                     /*NOP  is a special REXX statement.    */
signal event?                                    /*see if another event has happened.   */
```

'''output'''   when using the input of:   <tt> 1   3   5   0   7   9 </tt>

```txt

an event occurred at 16:13:29, the event is: 9
an event occurred at 16:13:30, the event is: 0
an event occurred at 16:13:31, the event is: 1
an event occurred at 16:13:33, the event is: 3
an event occurred at 16:13:35, the event is: 5
an event occurred at 16:13:37, the event is: 7
an event occurred at 16:13:39, the event is: 9
an event occurred at 16:13:40, the event is: 0
an event occurred at 16:13:41, the event is: 1
an event occurred at 16:13:43, the event is: 3
an event occurred at 16:13:45, the event is: 5
an event occurred at 16:13:47, the event is: 7
an event occurred at 16:13:49, the event is: 9
an event occurred at 16:13:50, the event is: 0
an event occurred at 16:13:51, the event is: 1
an event occurred at 16:13:53, the event is: 3
════════════ program halted.

```



## Rust


Rust ensures memory safety at compile-time without needing a garbage collector or runtime. There are several concurrency primitives in it's standard library.


```Rust

use std::{sync::mpsc, thread, time::Duration};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("[1] Starting");
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        println!("[2] Waiting for event");
        rx.recv();
        println!("[2] Received event");
    });
    thread::sleep(Duration::from_secs(1));
    println!("[1] Sending event");
    tx.send(())?;
    thread::sleep(Duration::from_secs(1));

    Ok(())
}

```



## Tcl

Tcl has been event-driven since 7.5, but only supported channel and timer events (plus variable traces, which can be used to create event-like entitites). With the addition of coroutines, it becomes much simpler to create general events:
{{works with|Tcl|8.6}}

```tcl
# Simple task framework built from coroutines
proc pause ms {
    after $ms [info coroutine];yield
}
proc task {name script} {
    coroutine $name apply [list {} \
        "set ::tasks(\[info coro]) 1;$script;unset ::tasks(\[info coro])"]
}
proc waitForTasksToFinish {} {
    global tasks
    while {[array size tasks]} {
	vwait tasks
    }
}

# Make an Ada-like event class
oo::class create Event {
    variable waiting fired
    constructor {} {
	set waiting {}
	set fired 0
    }
    method wait {} {
	while {!$fired} {
	    lappend waiting [info coroutine]
	    yield
	}
    }
    method signal {} {
	set wake $waiting
	set waiting {}
	set fired 1
	foreach task $wake {
	    $task
	}
    }
    method reset {} {
	set fired 0
    }
}

# Execute the example
Event create X
task A {
    puts "waiting for event"
    X wait
    puts "received event"
}
task B {
    pause 1000
    puts "signalling X"
    X signal
}
waitForTasksToFinish
```

Output:

```txt
waiting for event
signalling X
received event
```

Of course, the classic way of writing this is much shorter, but intermingles the tasks:

```tcl
after 1000 set X signalled
puts "waiting for event"
vwait X
puts "received event"
```



## zkl

zkl provides an Atomics library for things like this. Events are async, waiting for an event doesn't poll.

```zkl
var event=Atomic.Bool();  // False
   // create thread waiting for event
fcn(event){event.wait(); println(vm," ping!")}.launch(event);
Atomic.sleep(1);
event.set();
println("done")
```

{{out}}

```txt

// snooze
done   // setting is fast, receiving maybe not so
VM#4 ping!
// and thread 4 exits

```

I ran this from the REPL so I didn't have to worry about the main thread exiting and nuking the child thread.

{{omit from|GUISS}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC}}
{{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
