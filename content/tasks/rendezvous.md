+++
title = "Rendezvous"
description = ""
date = 2019-01-06T05:22:52Z
aliases = []
[extra]
id = 3559
[taxonomies]
categories = ["task", "Concurrency"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "c",
  "d",
  "erlang",
  "go",
  "julia",
  "oz",
  "perl_6",
  "phix",
  "picolisp",
  "racket",
  "tcl",
  "zkl",
]
+++

## Task

Demonstrate the “rendezvous” communications technique by implementing a printer monitor.
==Detailed Description of Programming Task==
Rendezvous is a synchronization mechanism based on procedural decomposition. Rendezvous is similar to a procedure call with the difference that the caller and the callee belong to different [[task]]s. The called procedure is usually called an '''entry point''' of the corresponding task. A call to an entry point is synchronous, i.e. the caller is blocked until completion. For the caller a call to the entry point is indivisible. Internally it consists of:

* Waiting for the callee ready to accept the rendezvous;
* Engaging the rendezvous (servicing the entry point).

The caller may limit the waiting time to the callee to accept the rendezvous. I.e. a rendezvous request can be aborted if not yet accepted by the callee. When accepted the rendezvous is processed until its completion. During this time the caller and the callee tasks stay synchronized. Which context is used to process the rendezvous depends on the implementation which may wish to minimize context switching.

The callee task may accept several rendezvous requests:

* Rendezvous to the same entry point from different tasks;
* Rendezvous to different entry points.

The callee accepts one rendezvous at a time.

Language mechanism of [[exceptions]] (if any) has to be consistent with the rendezvous. In particular when an exception is propagated out of a rendezvous it shall do in both tasks. The exception propagation is synchronous within the rendezvous and asynchronous outside it.

An engaged rendezvous can be requeued by the callee to another entry point of its task or to another task, transparently to the caller.

Differently to messages which are usually asynchronous, rendezvous are synchronous, as it was stated before. Therefore a rendezvous does not require marshaling the parameters and a buffer to keep them. Further, rendezvous can be implemented without context switch. This makes rendezvous a more efficient than messaging.

Rendezvous can be used to implement monitor synchronization objects. A monitor guards a shared resource. All users of the resource request a rendezvous to the monitor in order to get access to the resource. Access is granted by accepting the rendezvous for the time while the rendezvous is serviced.


### Language task

Show how rendezvous are supported by the language. If the language does not have rendezvous, provide an implementation of them based on other primitives.


### Use case task

Implement a printer monitor. The monitor guards a printer. There are two printers ''main'' and ''reserve''. Each has a monitor that accepts a rendezvous Print with a text line to print of the printer. The standard output may serve for printing purpose. Each character of the line is printed separately in order to illustrate that lines are printed indivisibly. Each printer has ink for only 5 lines of text. When the ''main'' printer runs out of ink it redirects its requests to the ''reserve'' printer. When that runs out of ink too, Out_Of_Ink exception propagates back to the caller. Create two writer tasks which print their plagiarisms on the printer. One does ''Humpty Dumpty'', another ''Mother Goose''.


## Ada

Ada has integrated [http://www.iuma.ulpgc.es/users/jmiranda/gnat-rts/node20.htm rendezvous support]. The caller calls to a rendezvous using the name of the task suffixed by the entry point name and the parameters. An entry point can be called using timed entry call statement which allow limit waiting time:

```ada
select
   Server.Wake_Up (Parameters);
or delay 5.0;
   -- No response, try something else
   ...
end select;
```

The task accepts a rendezvous using accept statement. The statement can contain body which implements the rendezvous. When several rendezvous need to be accepted a selective accept statement can be used. For example:

```ada
select
   accept Wake_Up (Parameters : Work_Item) do
      Current_Work_Item := Parameters;
   end;
   Process (Current_Work_Item);
or accept Shut_Down;
   exit;       -- Shut down requested
end select;
```

Entry points in the selective accept can be guarded by Boolean expressions which close the entry point when the expression yield false.

A task may requeue rendezvous request from the body of an accept statement to an entry point of the same or another task if the parameter profile of the entry point is compatible. The requeue statement may contain clause '''with abort'' which allows the caller to abort the request when it waits for other task to accept it. Without the clause the request is protected from abortion. This might be useful when the first task initiates processing of the request and the side effect of this action need to be removed when processing is completed.

### The task


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Rendezvous is
   Out_Of_Ink : exception;

   type Printer;
   type Printer_Ptr is access all Printer;
   task type Printer (ID : Natural; Backup : Printer_Ptr) is
      entry Print (Line : String);
   end Printer;

   task body Printer is
      Ink : Natural := 5;
   begin
      loop
         begin
            select
               accept Print (Line : String) do
                  if Ink = 0 then
                     if Backup = null then
                        raise Out_Of_Ink;
                     else
                        requeue Backup.Print with abort;
                     end if;
                  else
                     Put (Integer'Image (ID) & ": ");
                     for I in Line'Range loop
                        Put (Line (I));
                     end loop;
                     New_Line;
                     Ink := Ink - 1;
                  end if;
               end Print;
            or terminate;
            end select;
         exception
            when Out_Of_Ink =>
               null;
         end;
      end loop;
   end Printer;

   Reserve : aliased Printer (2, null);
   Main    : Printer (1, Reserve'Access);

   task Humpty_Dumpty;
   task Mother_Goose;

   task body Humpty_Dumpty is
   begin
      Main.Print ("Humpty Dumpty sat on a wall.");
      Main.Print ("Humpty Dumpty had a great fall.");
      Main.Print ("All the king's horses and all the king's men");
      Main.Print ("Couldn't put Humpty together again.");
   exception
      when Out_Of_Ink =>
         Put_Line ("      Humpty Dumpty out of ink!");
   end Humpty_Dumpty;

   task body Mother_Goose is
   begin
      Main.Print ("Old Mother Goose");
      Main.Print ("When she wanted to wander,");
      Main.Print ("Would ride through the air");
      Main.Print ("On a very fine gander.");
      Main.Print ("Jack's mother came in,");
      Main.Print ("And caught the goose soon,");
      Main.Print ("And mounting its back,");
      Main.Print ("Flew up to the moon.");
   exception
      when Out_Of_Ink =>
         Put_Line ("      Mother Goose out of ink!");
   end Mother_Goose;

begin
   null;
end Rendezvous;
```

Sample output:

```txt

 1: Old Mother Goose
 1: Humpty Dumpty sat on a wall.
 1: When she wanted to wander,
 1: Humpty Dumpty had a great fall.
 1: Would ride through the air
 2: All the king's horses and all the king's men
 2: On a very fine gander.
 2: Couldn't put Humpty together again.
 2: Jack's mother came in,
 2: And caught the goose soon,
      Mother Goose out of ink!

```


## AutoHotkey


```AutoHotkey
OnMessage(0x4a, "PrintMonitor")
SetTimer, print2, 400

print1:
  print("Old Mother Goose")
  print("When she wanted to wander,")
  print("Would ride through the air")
  print("On a very fine gander.")
  print("Jack's mother came in,")
  print("And caught the goose soon,")
  print("And mounting its back,")
  print("Flew up to the moon.")
Return

print2:
  SetTimer, print2, Off
  print("Humpty Dumpty sat on a wall.")
  print("Humpty Dumpty had a great fall.")
  print("All the king's horses and all the king's men")
  print("Couldn't put Humpty together again.")
Return

print(message)
{
  Static StringToSend
  StringToSend := message
  Gui +LastFound
  VarSetCapacity(CopyDataStruct, 12, 0)
  NumPut(StrLen(StringToSend) + 1, CopyDataStruct, 4)
  NumPut(&StringToSend, CopyDataStruct, 8)
  SendMessage, 0x4a, 0, &CopyDataStruct
  If ErrorLevel
    MsgBox out of ink
  Sleep, 200
  Return
}

PrintMonitor(wParam, lParam, msg)
{
  Static ink = 5
  Global printed
  Critical
  If ink
  {
    StringAddress := NumGet(lParam + 8)
    StringLength := DllCall("lstrlen", UInt, StringAddress)
    VarSetCapacity(CopyOfData, StringLength)
    DllCall("lstrcpy", "str", CopyOfData, "uint", StringAddress)
    printed .= "primaryprinter: " . CopyOfData . "`n"
    ToolTip, primary printer`n: %printed%
    ink--
  }
  Else
  {
    OnMessage(0x4a, "Reserve")
    print(CopyOfData)
  }
}

Reserve(wParam, lParam, msg)
{
  Static ink = 5
  Global printed
  Critical
  If ink
  {
    StringAddress := NumGet(lParam + 8)
    StringLength := DllCall("lstrlen", UInt, StringAddress)
    VarSetCapacity(CopyOfData, StringLength)
    DllCall("lstrcpy", "str", CopyOfData, "uint", StringAddress)
    printed .= "reserveprinter: " . CopyOfData . "`n"
    ToolTip, Reserve printer`n: %printed%
    ink--
  }
  Else
    Return -1
}
```


## C

Basically just synched threads doing printing: since task didn't ask for service type or resource enumeration, and "message passing is stupid" (c.f. talk), the guarding thread is no more than a glorified mutex, hence completely cut out, leaving the threads directly check ink and do print.

```c
#include <stdio.h>
#include <unistd.h>
#include <omp.h>

typedef struct printer printer;
struct printer { int id, ink; };
printer pnt_main = { 1, 5 };
printer pnt_backup = { 2, 5 };

int print(const char * text, const char **error)
{
	#pragma omp critical
	{
		printer *p = &pnt_main;
		if (!p->ink) p = &pnt_backup;
		if (!p->ink)
			*error = "Out of ink";
		else {
			*error = 0;
			p->ink--;
			printf("%d | ", p->id, p->ink);
			while (*text != '\0') {
				putchar(*(text++));
				fflush(stdout);
				usleep(30000);
			}
			putchar('\n');
		}
	}
	return 0 != *error;
}

const char *humpty[] = {
	"Humpty Dumpty sat on a wall.",
	"Humpty Dumpty had a great fall.",
	"All the king's horses and all the king's men,",
	"Couldn't put Humpty together again."
};

const char *goose[] = {
	"Old Mother Goose,",
	"When she wanted to wander,",
	"Would ride through the air,",
	"On a very fine gander.",
	"Jack's mother came in,",
	"And caught the goose soon,",
	"And mounting its back,",
	"Flew up to the moon."
};

int main()
{
	int i, j, len;
	const char *msg, **text;

	omp_set_num_threads(2);

	#pragma omp parallel for private(text, msg, len, j)
	for (i = 0; i < 2; i++) {
		text = i ? goose : humpty;
		len = (i ? sizeof(goose) : sizeof(humpty) ) / sizeof(const char*);
		for (j = 0; j < len; j++) {
			usleep(100000);
			if (print(text[j], &msg)) {
				fprintf(stderr, "Error: %s\n", msg);
				break;
			}
		}
	}

	return 0;
}
```



## D


```d
import std.stdio, std.array, std.datetime, std.exception,
       std.concurrency, core.thread, core.atomic;

final class OutOfInk: Exception {
    this() pure nothrow {
        super("Out of ink.");
    }
}

struct Printer {
    string id;
    size_t ink;

    void printIt(in string line) {
        enforce(ink != 0, new OutOfInk);
        writefln("%s: %s", id, line);
        ink--;
    }
}

/// Treats shared lvalue as if it is thread-local.
ref assumeThreadLocal(T)(ref shared T what) pure nothrow {
    return *cast(T*)&what;
}

struct RendezvousPrinter {
    Printer[] printers;

    void print(const(string)[] lines) shared {
        OutOfInk savedException;

        // Lightweight mutual exclusion
        // using shared atomic bool.
        shared static bool mutex;

        void lock() {
            while (!cas(&mutex, false, true))
                Thread.sleep(1.hnsecs);
        }

        void unlock() nothrow {
            assert(mutex.atomicLoad!(MemoryOrder.acq));
            mutex.atomicStore!(MemoryOrder.rel)(false);
        }

        while (!lines.empty) {
            if (printers.empty) {
                // No more printers to try.
                assert(savedException !is null);
                throw savedException;
            }

            try {
                {
                    lock;
                    scope(exit) unlock;
                    printers.front.assumeThreadLocal
                        .printIt(lines.front);
                }
                lines.popFront;

                // Increase the chance of interleaved output.
                Thread.sleep(10.msecs);
            } catch (OutOfInk exc) {
                savedException = exc;

                // Switch to the next printer.
                lock;
                scope(exit) unlock;
                printers.assumeThreadLocal.popFront;
            }
        }
    }
}

void humptyDumptyTask(shared ref RendezvousPrinter rendezvous) {
    const humptyDumpty = [
        "Humpty Dumpty sat on a wall.",
        "Humpty Dumpty had a great fall.",
        "All the king's horses and all the king's men,",
        "Couldn't put Humpty together again."];

    rendezvous.print(humptyDumpty);
}

void motherGooseTask(shared ref RendezvousPrinter rendezvous) {
    const motherGoose = ["Old Mother Goose,",
                         "When she wanted to wander,",
                         "Would ride through the air,",
                         "On a very fine gander.",
                         "Jack's mother came in,",
                         "And caught the goose soon,",
                         "And mounting its back,",
                         "Flew up to the moon."];

    rendezvous.print(motherGoose);
}

void main() {
    shared rendezvous = RendezvousPrinter
        ([Printer("main", 5), Printer("reserve", 5)]);

    spawn(&humptyDumptyTask, rendezvous);
    spawn(&motherGooseTask, rendezvous);
}
```

```txt
main: Humpty Dumpty sat on a wall.
main: Old Mother Goose,
main: When she wanted to wander,
main: Humpty Dumpty had a great fall.
main: Would ride through the air,
reserve: On a very fine gander.
reserve: All the king's horses and all the king's men,
reserve: Jack's mother came in,
reserve: And caught the goose soon,
reserve: Couldn't put Humpty together again.
rendezvous2.OutOfInk @rendezvous2.d(6): Out of ink.
----------------
0x0040609F
0x004021FC
0x0040243A
0x00405F47
0x00412D55
0x00433A48
0x77BB1603 in RtlInitializeExceptionChain
0x77BB15D6 in RtlInitializeExceptionChain
```



## Erlang

There is no rendezvous in Erlang. To fulfil the task description I have implemented rendezvous with message passing (which is in Erlang). Doing these printers directly with message passing would have been simpler (in Erlang).


```Erlang

-module( rendezvous ).

-export( [task/0] ).

task() ->
    Printer_pid = erlang:spawn( fun() -> printer(1, 5) end ),
    Reserve_printer_pid = erlang:spawn( fun() -> printer(2, 5) end ),
    Monitor_pid = erlang:spawn( fun() -> printer_monitor(Printer_pid, Reserve_printer_pid) end ),
    erlang:spawn( fun() -> print(Monitor_pid, humpty_dumpty()) end ),
    erlang:spawn( fun() -> print(Monitor_pid, mother_goose()) end ).

humpty_dumpty() ->
        ["Humpty Dumpty sat on a wall.",
                "Humpty Dumpty had a great fall.",
                "All the king's horses and all the king's men,",
                "Couldn't put Humpty together again."].

mother_goose() ->
        ["Old Mother Goose,",
               "When she wanted to wander,",
               "Would ride through the air,",
               "On a very fine gander.",
               "Jack's mother came in,",
               "And caught the goose soon,",
               "And mounting its back,",
               "Flew up to the moon."].

print( Pid, Lines ) ->
    io:fwrite( "Print ~p started~n", [erlang:self()] ),
    print( Pid, Lines, infinity ).

print( _Pid, [], _Timeout ) -> ok;
print( Pid, [Line | T], Timeout ) ->
    print_line( Pid, Line, Timeout ),
    print_line_done(),
    print( Pid, T, Timeout ).

print_line( Pid, Line, Timeout ) ->
    Pid ! {print, Line, erlang:self()},
    receive
    {print, started} -> ok
    after Timeout -> erlang:throw( timeout )
    end.

print_line_done() ->
    receive
    {printer, ok} -> ok;
    {printer, out_of_ink} -> erlang:throw( out_of_ink )
    end.

printer( N, 0 ) ->
        receive
        {print, _Line, Pid} -> Pid ! {printer, out_of_ink}
        end,
        printer( N, 0 );
printer( N, Ink ) ->
        receive
        {print, Line, Pid} ->
                Pid ! {printer, ok},
                io:fwrite( "~p: ", [N] ),
                [io:fwrite("~c", [X]) || X <- Line],
                io:nl()
        end,
        printer( N, Ink - 1 ).

printer_monitor( Printer, Reserve ) ->
        {Line, Pid} = printer_monitor_get_line(),
        Result = printer_monitor_print_line( Printer, Line ),
        printer_monitor_reserve( Result, Reserve, Line, Pid ),
        printer_monitor( Printer, Reserve ).

printer_monitor_get_line() ->
        receive
        {print, Line, Pid} ->
                Pid ! {print, started},
                {Line, Pid}
        end.

printer_monitor_print_line( Printer_pid, Line ) ->
        Printer_pid ! {print, Line, erlang:self()},
        receive
        {printer, Result} -> Result
        end.

printer_monitor_reserve( ok, _Reserve_pid, _Line, Pid ) -> Pid ! {printer, ok};
printer_monitor_reserve( out_of_ink, Reserve_pid, Line, Pid ) -> Reserve_pid ! {print, Line, Pid}.

```

The first printouts are there to show the identity of the processes that print. It makes it easier to match the exception to one of them and not to some other process.

```txt

53> rendezvous:task().
Print <0.251.0> started
Print <0.252.0> started
1: Humpty Dumpty sat on a wall.
1: Old Mother Goose,
1: Humpty Dumpty had a great fall.
1: When she wanted to wander,
1: All the king's horses and all the king's men,
2: Would ride through the air,
2: Couldn't put Humpty together again.
2: On a very fine gander.
2: Jack's mother came in,
2: And caught the goose soon,

=ERROR REPORT==== 22-Sep-2013::12:09:56 ===
Error in process <0.252.0> with exit value: {{nocatch,out_of_ink},[{rendezvous,print_line_done,0,[{file,"rendezvous.erl"},{line,48}]},{rendezvous,print,3,[{file,"rendezvous.erl"},{line,35}]}]}

```


=={{header|F_Sharp|F#}}==

The rendezvous mechanism is realized by using F#'s mailbox processors to implement active objects.

It is possible to extract the boilerplate code into a reusable helper class which should be considered when using active objects a lot.


```fsharp
open System

type PrinterCommand = Print of string

// a message is a command and a facility to return an exception
type Message = Message of PrinterCommand * AsyncReplyChannel<Exception option>

// thrown if we have no more ink (and neither has our possible backup printer)
exception OutOfInk

type Printer(id, ?backup:Printer) =
   let mutable ink = 5

   // the actual printing logic as a private function
   let print line =
      if ink > 0 then
         printf "%d: " id
         Seq.iter (printf "%c") line
         printf "\n"
         ink <- ink - 1
      else
         match backup with
         | Some p -> p.Print line
         | None -> raise OutOfInk

   // use a MailboxProcessor to process commands asynchronously;
   // if an exception occurs, we return it to the calling thread
   let agent = MailboxProcessor.Start( fun inbox ->
      async {
         while true do
            let! Message (command, replyChannel) = inbox.Receive()
            try
               match command with
               | Print line -> print line
               replyChannel.Reply None
            with
               | ex -> replyChannel.Reply (Some ex)
      })

   // public printing method:
   // send Print command and propagate exception if one occurs
   member x.Print line =
      match agent.PostAndReply( fun replyChannel -> Message (Print line, replyChannel) ) with
      | None -> ()
      | Some ex -> raise ex


open System.Threading

do
  let main = new Printer(id=1, backup=new Printer(id=2))

  (new Thread(fun () ->
      try
        main.Print "Humpty Dumpty sat on a wall."
        main.Print "Humpty Dumpty had a great fall."
        main.Print "All the king's horses and all the king's men"
        main.Print "Couldn't put Humpty together again."
      with
        | OutOfInk -> printfn "      Humpty Dumpty out of ink!"
  )).Start()

  (new Thread(fun () ->
      try
        main.Print "Old Mother Goose"
        main.Print "Would ride through the air"
        main.Print "On a very fine gander."
        main.Print "Jack's mother came in,"
        main.Print "And caught the goose soon,"
        main.Print "And mounting its back,"
        main.Print "Flew up to the moon."
      with
        | OutOfInk -> printfn "      Mother Goose out of ink!"
  )).Start()

  Console.ReadLine() |> ignore
```


Example output:

```txt
1: Old Mother Goose
1: Humpty Dumpty sat on a wall.
1: Would ride through the air
1: Humpty Dumpty had a great fall.
1: On a very fine gander.
2: All the king's horses and all the king's men
2: Jack's mother came in,
2: Couldn't put Humpty together again.
2: And caught the goose soon,
2: And mounting its back,
      Mother Goose out of ink!
```



## Go


```go
package main

import (
    "errors"
    "fmt"
    "strings"
    "sync"
)

var hdText = `Humpty Dumpty sat on a wall.
Humpty Dumpty had a great fall.
All the king's horses and all the king's men,
Couldn't put Humpty together again.`

var mgText = `Old Mother Goose,
When she wanted to wander,
Would ride through the air,
On a very fine gander.
Jack's mother came in,
And caught the goose soon,
And mounting its back,
Flew up to the moon.`

func main() {
    reservePrinter := startMonitor(newPrinter(5), nil)
    mainPrinter := startMonitor(newPrinter(5), reservePrinter)
    var busy sync.WaitGroup
    busy.Add(2)
    go writer(mainPrinter, "hd", hdText, &busy)
    go writer(mainPrinter, "mg", mgText, &busy)
    busy.Wait()
}

// printer is a type representing an abstraction of a physical printer.
// It is a type defintion for a function that takes a string to print
// and returns an error value, (hopefully usually nil, meaning no error.)
type printer func(string) error

// newPrinter is a constructor.  The parameter is a quantity of ink.  It
// returns a printer object encapsulating the ink quantity.
// Note that this is not creating the monitor, only the object serving as
// a physical printer by writing to standard output.
func newPrinter(ink int) printer {
    return func(line string) error {
        if ink == 0 {
            return eOutOfInk
        }
        for _, c := range line {
            fmt.Printf("%c", c)
        }
        fmt.Println()
        ink--
        return nil
    }
}

var eOutOfInk = errors.New("out of ink")

// For the language task, rSync is a type used to approximate the Ada
// rendezvous mechanism that includes the caller waiting for completion
// of the callee.  For this use case, we signal completion with an error
// value as a response.  Exceptions are not idiomatic in Go and there is
// no attempt here to model the Ada exception mechanism.  Instead, it is
// idomatic in Go to return error values.  Sending an error value on a
// channel works well here to signal completion.  Go unbuffered channels
// provide synchronous rendezvous, but call and response takes two channels,
// which are bundled together here in a struct.  The channel types are chosen
// to mirror the parameter and return types of "type printer" defined above.
// The channel types here, string and error are both "reference types"
// in Go terminology.  That is, they are small things containing pointers
// to the actual data.  Sending one on a channel does not involve copying,
// or much less marshalling string data.
type rSync struct {
    call     chan string
    response chan error
}

// "rendezvous Print" requested by use case task.
// For the language task though, it is implemented here as a method on
// rSync that sends its argument on rSync.call and returns the result
// received from rSync.response.  Each channel operation is synchronous.
// The two operations back to back approximate the Ada rendezvous.
func (r *rSync) print(data string) error {
    r.call <- data      // blocks until data is accepted on channel
    return <-r.response // blocks until response is received
}

// monitor is run as a goroutine.  It encapsulates the printer passed to it.
// Print requests are received through the rSync object "entry," named entry
// here to correspond to the Ada concept of an entry point.
func monitor(hardPrint printer, entry, reserve *rSync) {
    for {
        // The monitor goroutine will block here waiting for a "call"
        // to its "entry point."
        data := <-entry.call
        // Assuming the call came from a goroutine calling rSync.print,
        // that goroutine is now blocked, waiting for this one to send
        // a response.

        // attempt output
        switch err := hardPrint(data); {

        // consider return value from attempt
        case err == nil:
            entry.response <- nil // no problems

        case err == eOutOfInk && reserve != nil:
            // Requeue to "entry point" of reserve printer monitor.
            // Caller stays blocked, and now this goroutine blocks until
            // it gets a response from the reserve printer monitor.
            // It then transparently relays the response to the caller.
            entry.response <- reserve.print(data)

        default:
            entry.response <- err // return failure
        }
        // The response is away.  Loop, and so immediately block again.
    }
}

// startMonitor can be seen as an rSync constructor.  It also
// of course, starts the monitor for which the rSync serves as entry point.
// Further to the langauge task, note that the channels created here are
// unbuffered.  There is no buffer or message box to hold channel data.
// A sender will block waiting for a receiver to accept data synchronously.
func startMonitor(p printer, reservePrinter *rSync) *rSync {
    entry := &rSync{make(chan string), make(chan error)}
    go monitor(p, entry, reservePrinter)
    return entry
}

// Two writer tasks are started as goroutines by main.  They run concurrently
// and compete for printers as resources.  Note the call to "rendezvous Print"
// as requested in the use case task and compare the syntax,
//    Here:           printMonitor.print(line);
//    Ada solution:   Main.Print ("string literal");
func writer(printMonitor *rSync, id, text string, busy *sync.WaitGroup) {
    for _, line := range strings.Split(text, "\n") {
        if err := printMonitor.print(line); err != nil {
            fmt.Printf("**** writer task %q terminated: %v ****\n", id, err)
            break
        }
    }
    busy.Done()
}
```

Output:

```txt

Humpty Dumpty sat on a wall.
Old Mother Goose,
Humpty Dumpty had a great fall.
When she wanted to wander,
All the king's horses and all the king's men,
Would ride through the air,
Couldn't put Humpty together again.
On a very fine gander.
Jack's mother came in,
And caught the goose soon,
**** writer task "mg" terminated: out of ink ****

```



## Julia

Julia has coroutines started with the @async macro and Channels, which can be used for interprocess communication, such as passing lines to and errors from a printing routine.

```julia
mutable struct Printer
    inputpath::Channel{String}
    errorpath::Channel{String}
    inkremaining::Int32
    reserve::Printer
    name::String
    function Printer(ch1, ch2, ink, name)
        this = new()
        this.inputpath = ch1
        this.errorpath = ch2
        this.inkremaining = ink
        this.name = name
        this.reserve = this
        this
    end
end

function lineprintertask(printer)
    while true
        line = take!(printer.inputpath)
        linesprinted = 0
        if(printer.inkremaining < 1)
            if(printer.reserve == printer)
                put!(printer.errorpath, "Error: printer $(printer.name) out of ink")
            else
                put!(printer.reserve.inputpath, line)
            end
        else
            println(line)
            printer.inkremaining -= 1
        end
    end
end

function schedulework(poems)
    printerclose(printer) = (close(printer.inputpath); close(printer.errorpath))
    reserveprinter = Printer(Channel{String}(1), Channel{String}(10), 5, "Reserve")
    mainprinter = Printer(Channel{String}(1), Channel{String}(10), 5, "Main")
    mainprinter.reserve = reserveprinter
    @async(lineprintertask(mainprinter))
    @async(lineprintertask(reserveprinter))
    printers = [mainprinter, reserveprinter]
    activeprinter = 1
    @sync(
    for poem in poems
        activeprinter = (activeprinter % length(printers)) + 1
        @async(
        for line in poem
            put!(printers[activeprinter].inputpath, line)
        end)
    end)
    for p in printers
        while isready(p.errorpath)
            println(take!(p.errorpath))
        end
        printerclose(p)
    end
end

const humptydumpty = ["Humpty Dumpty sat on a wall.",
                      "Humpty Dumpty had a great fall.",
                      "All the king's horses and all the king's men,",
                      "Couldn't put Humpty together again."]

const oldmothergoose = ["Old Mother Goose,",
                        "When she wanted to wander,",
                        "Would ride through the air,",
                        "On a very fine gander.",
                        "Jack's mother came in,",
                        "And caught the goose soon,",
                        "And mounting its back,",
                        "Flew up to the moon."]

schedulework([humptydumpty, oldmothergoose])

```
```txt

 Humpty Dumpty sat on a wall.
 Humpty Dumpty had a great fall.
 Old Mother Goose,
 All the king's horses and all the king's men,
 When she wanted to wander,
 Couldn't put Humpty together again.
 Would ride through the air,
 On a very fine gander.
 Jack's mother came in,
 And caught the goose soon,
 Error: printer Reserve out of ink
 Error: printer Reserve out of ink

```



## Oz

Oz does not have a rendezvous mechanism, but the task description lends itself to synchronous [http://c2.com/cgi/wiki?ActiveObject active objects]. We show how to implement this in Oz and then discuss the differences to the rendezvous model.

First a simple printer class whose definition is completely orthogonal to multithreading issues:

```oz
declare
  class Printer
     attr ink:5

     feat id backup

     meth init(id:ID backup:Backup<=unit)
        self.id = ID
        self.backup = Backup
     end

     meth print(Line)=Msg
        if @ink == 0 then
           if self.backup == unit then
              raise outOfInk end
           else
              {self.backup Msg}
           end
        else
           {System.printInfo self.id#": "}
           for C in Line do
              {System.printInfo [C]}
           end
           {System.printInfo "\n"}
           ink := @ink - 1
        end
     end
  end
```

Note how requeuing the task simply becomes delegation to a different object.

Active object are not a predefined abstraction in Oz. But due to Oz' first-class object messages, we can easily define it using ports and streams (many-to-one message passing):

```oz
  fun {NewActiveSync Class Init}
     Obj = {New Class Init}
     MsgPort
  in
     thread MsgStream in
        {NewPort ?MsgStream ?MsgPort}
        for Msg#Sync in MsgStream do
           try
              {Obj Msg}
              Sync = unit
           catch E then
              Sync = {Value.failed E}
           end
        end
     end
     proc {$ Msg}
        Sync = {Port.sendRecv MsgPort Msg}
     in
        {Wait Sync}
     end
  end
```

This functions takes a class and an initialization message and returns a procedure. When called, this procedure will send messages to the new object in a new thread and then wait for the <code>Sync</code> variable to become bound. Exceptions are propagated using [http://www.mozart-oz.org/home/doc/base/node13.html#label696 failed values].

This works because a unary procedure is syntactically indistinguishable from an object in Oz.

With this new abstraction we can create the two printers and execute both print tasks in their own thread:

```oz
  Main = {NewActiveSync Printer init(id:1 backup:Reserve)}
  Reserve = {NewActiveSync Printer init(id:2)}
in
  %% task Humpty Dumpty
  thread
     try
	{Main print("Humpty Dumpty sat on a wall.")}
	{Main print("Humpty Dumpty had a great fall.")}
	{Main print("All the king's horses and all the king's men")}
	{Main print("Couldn't put Humpty together again.")}
     catch outOfInk then
	{System.showInfo "      Humpty Dumpty out of ink!"}
     end
  end

  %% task Mother Goose
  thread
     try
	{Main print("Old Mother Goose")}
	{Main print("When she wanted to wander,")}
	{Main print("Would ride through the air")}
	{Main print("On a very fine gander.")}
	{Main print("Jack's mother came in,")}
	{Main print("And caught the goose soon,")}
	{Main print("And mounting its back,")}
	{Main print("Flew up to the moon.")}
     catch outOfInk then
	{System.showInfo "      Mother Goose out of ink!"}
     end
  end
```


Example output:

```txt

1: Humpty Dumpty sat on a wall.
1: Old Mother Goose
1: Humpty Dumpty had a great fall.
1: When she wanted to wander,
1: All the king's horses and all the king's men
2: Would ride through the air
2: Couldn't put Humpty together again.
2: On a very fine gander.
2: Jack's mother came in,
2: And caught the goose soon,
      Mother Goose out of ink!

```


'''Comparison to Ada'''

What is called an "entry point" in Ada, is a method in our implementation.

We cannot limit the waiting time. This could be implemented in the NewActiveSync function, but it is not needed in the example task.

The callee task accepts rendezvous requests to the same entry point from multiple caller tasks. Similarly, the active object in Oz is designed to accept messages from different threads. To implement "rendezvous to different entry points", we simply add public methods to the Printer class.

The callee in ADA accepts one rendezvous at a time. The active object in Oz reads one message at a time from the stream.

Messages cannot be requeued in the given implementation. But we can delegate to a different active object which has the same effect, at least for the example given.

Like in the rendezvous mechanism, parameters are not marshalled. This is because sharing immutable data between threads is safe.
In contrast to ADA, the parameters are buffered until the printer becomes ready. But with a synchronous communication mechanism, this should not cause problems.


## Perl 6


Perl 6 has no built-in support for rendezvous. I tried to simulate it using message passing and a lock - not sure if that counts.

```perl6
class X::OutOfInk is Exception {
    method message() { "Printer out of ink" }
}

class Printer {
    has Str      $.id;
    has Int      $.ink = 5;
    has Lock     $!lock .= new;
    has ::?CLASS $.fallback;

    method print ($line) {
        $!lock.protect: {
            if    $!ink      { say "$!id: $line"; $!ink-- }
            elsif $!fallback { $!fallback.print: $line }
            else             { die X::OutOfInk.new }
        }
    }
}

my $printer =
    Printer.new: id => 'main', fallback =>
    Printer.new: id => 'reserve';

sub client ($id, @lines) {
    start {
        for @lines {
            $printer.print: $_;
            CATCH {
                when X::OutOfInk { note "<$id stops for lack of ink>"; exit }
            }
        }
        note "<$id is done>";
    }
}

await
    client('Humpty', q:to/END/.lines),
        Humpty Dumpty sat on a wall.
        Humpty Dumpty had a great fall.
        All the king's horses and all the king's men,
        Couldn't put Humpty together again.
        END
    client('Goose', q:to/END/.lines);
        Old Mother Goose,
        When she wanted to wander,
        Would ride through the air,
        On a very fine gander.
        Jack's mother came in,
        And caught the goose soon,
        And mounting its back,
        Flew up to the moon.
        END
```


```txt

main: Humpty Dumpty sat on a wall.
main: Old Mother Goose,
main: Humpty Dumpty had a great fall.
main: When she wanted to wander,
main: All the king's horses and all the king's men,
reserve: Would ride through the air,
reserve: Couldn't put Humpty together again.
reserve: On a very fine gander.
<Humpty is done>
reserve: Jack's mother came in,
reserve: And caught the goose soon,
<Goose stops for lack of ink>

```



## Phix

Phix has no rendezvous mechanism, the following achieves something similar using a simple mutex.

```Phix
constant print_cs = init_cs()
enum NAME,INK
sequence printers = {{"main",5},
                     {"reserve",5}}

procedure printer(string name, sequence s)
    try
        for i=1 to length(s) do
            enter_cs(print_cs)
            for p=1 to length(printers) do
                if printers[p][INK]!=0 then
                    printers[p][INK] -= 1
                    printf(1,"%s/%s: %s\n",{name,printers[p][NAME],s[i]})
                    exit
                elsif p=length(printers) then
                    throw("out of ink")
                end if
            end for
            leave_cs(print_cs)
        end for
        exit_thread(0)
    catch e
        printf(1,"exception(%s): %s\n",{name,e[E_USER]})
        leave_cs(print_cs)
        exit_thread(1)
    end try
end procedure
constant r_printer = routine_id("printer")

constant hd = {"Humpty Dumpty sat on a wall.",
               "Humpty Dumpty had a great fall.",
               "All the king's horses and all the king's men",
               "Couldn't put Humpty together again."},
         mg = {"Old Mother Goose",
               "When she wanted to wander,",
               "Would ride through the air",
               "On a very fine gander.",
               "Jack's mother came in,",
               "And caught the goose soon,",
               "And mounting its back,",
               "Flew up to the moon."}

sequence hThreads = {create_thread(r_printer,{"hd",hd}),
                     create_thread(r_printer,{"mg",mg})}
wait_thread(hThreads)
```

```txt

hd/main: Humpty Dumpty sat on a wall.
mg/main: Old Mother Goose
mg/main: When she wanted to wander,
hd/main: Humpty Dumpty had a great fall.
hd/main: All the king's horses and all the king's men
hd/reserve: Couldn't put Humpty together again.
mg/reserve: Would ride through the air
mg/reserve: On a very fine gander.
mg/reserve: Jack's mother came in,
mg/reserve: And caught the goose soon,
exception(mg): out of ink

```



## PicoLisp

Rendezvous can be implemented in PicoLisp via the following function:

```PicoLisp
(de rendezvous (Pid . Exe)
   (when
      (catch '(NIL)
         (tell Pid 'setq 'Rendezvous (lit (eval Exe)))
         NIL )
      (tell Pid 'quit @) ) )  # Raise caught error in caller
```

The caller invokes it in the callee via the
'[http://software-lab.de/doc/refT.html#tell tell]' interprocess communication,
and it uses 'tell' in turn to communicate results (and possible errors) back to
the caller.

Use case task:

```PicoLisp
(de printLine (Str)
   (cond
      ((gt0 *Ink) (prinl *ID ": " Str) (dec '*Ink))
      (*Backup (rendezvousPrint @ Str) T)
      (T (quit "Out of Ink")) ) )

(de rendezvousPrint (Printer Str)
   (let Rendezvous NIL
      (tell Printer 'rendezvous *Pid 'printLine Str)  # Call entry point
      (unless (wait 6000 Rendezvous)                  # Block max. 1 minute
         (quit "Rendezvous timed out") ) ) )

# Start RESERVE printer process
(unless (setq *ReservePrinter (fork))
   (setq *ID 2  *Ink 5)
   (wait) )  # Run forever

# Start MAIN printer process
(unless (setq *MainPrinter (fork))
   (setq *ID 1  *Ink 5  *Backup *ReservePrinter)
   (wait) )

# Start Humpty Dumpty process
(unless (fork)
   (when
      (catch '(NIL)
         (for Line
            (quote
               "Humpty Dumpty sat on a wall."
               "Humpty Dumpty had a great fall."
               "All the king's horses and all the king's men"
               "Couldn't put Humpty together again." )
            (rendezvousPrint *MainPrinter Line) ) )
      (prinl "      Humpty Dumpty: " @ "!") )
   (bye) )

# Start Mother Goose process
(unless (fork)
   (when
      (catch '(NIL)
         (for Line
            (quote
               "Old Mother Goose"
               "When she wanted to wander,"
               "Would ride through the air"
               "On a very fine gander."
               "Jack's mother came in,"
               "And caught the goose soon,"
               "And mounting its back,"
               "Flew up to the moon." )
            (rendezvousPrint *MainPrinter Line) ) )
      (prinl "      Mother Goose: " @ "!") )
   (bye) )

# Prepare to terminate all processes upon exit
(push '*Bye '(tell 'bye))
```

Output:

```txt
1: Old Mother Goose
1: Humpty Dumpty sat on a wall.
1: When she wanted to wander,
1: Humpty Dumpty had a great fall.
1: Would ride through the air
2: All the king's horses and all the king's men
2: On a very fine gander.
2: Jack's mother came in,
2: And caught the goose soon,
2: And mounting its back,
      Humpty Dumpty: Out of Ink!
```



## Racket


```Racket

#lang racket

;;; Rendezvous primitives implemented in terms of synchronous channels.
(define (send ch msg)
  (define handshake (make-channel))
  (channel-put ch (list msg handshake))
  (channel-get handshake)
  (void))

(define (receive ch action)
  (match-define (list msg handshake) (channel-get ch))
  (action msg)
  (channel-put handshake 'done))

;;; A printer receives a line of text, then
;;;   - prints it                      (still ink left)
;;;   - sends it to the backup printer (if present)
;;;   - raises exception               (if no ink and no backup)
(define (printer id ink backup)
  (define (on-line-received line)
    (cond
      [(and (= ink 0) (not backup)) (raise 'out-of-ink)]
      [(= ink 0)                    (send backup line)]
      [else                         (display (~a id ":"))
                                    (for ([c line]) (display c))
                                    (newline)]))
  (define ch (make-channel))
  (thread
   (λ ()
    (let loop ()
      (receive ch on-line-received)
      (set! ink (max 0 (- ink 1)))
      (loop))))
  ch)

;;; Setup two printers

(define reserve (printer "reserve" 5 #f))
(define main    (printer "main"    5 reserve))

;;; Two stories

(define humpty
  '("Humpty Dumpty sat on a wall."
	"Humpty Dumpty had a great fall."
	"All the king's horses and all the king's men,"
	"Couldn't put Humpty together again."))

(define goose
  '("Old Mother Goose,"
    "When she wanted to wander,"
    "Would ride through the air,"
	"On a very fine gander."
	"Jack's mother came in,"
	"And caught the goose soon,"
	"And mounting its back,"
	"Flew up to the moon."))

;;; Print the stories
(for ([l humpty]) (send main l))
(for ([l goose])  (send main l))

```


Output:

```Racket

main:Humpty Dumpty sat on a wall.
main:Humpty Dumpty had a great fall.
main:All the king's horses and all the king's men,
main:Couldn't put Humpty together again.
main:Old Mother Goose,
reserve:When she wanted to wander,
reserve:Would ride through the air,
reserve:On a very fine gander.
reserve:Jack's mother came in,
reserve:And caught the goose soon,
uncaught exception: 'out-of-ink

```



## Tcl

Tcl does not have a rendezvous operation, but it does have the ability to send a script to another thread to be evaluated and the results passed back. Combined with coroutines (so that the code is not too ugly), this can make something that works very much like a rendezvous operation.


```tcl
package require Tcl 8.6
package require Thread

# Really ought to go in a package
eval [set rendezvousEngine {
array set Select {w {} c 0}

# Turns the task into a coroutine, making it easier to write in "Ada style".
# The real thread ids are stored in shared variables.
proc task {id script} {
    global rendezvousEngine
    set task [list coroutine RTask eval "$script;thread::exit"]
    tsv::set tasks $id [thread::create \
		"$rendezvousEngine;$task;thread::wait"]
}

# A simple yielding pause.
proc pause t {
    after $t [info coroutine]
    yield
}

# Wait for a message. Note that this is *not* pretty code and doesn't do
# everything that the Ada rendezvous does.
proc select args {
    global Select
    set var [namespace which -variable Select](m[incr Select(c)])
    set messages {}
    foreach {message vars body} $args {
	dict set messages $message $body
	dict set bindings $message $vars
    }
    lappend Select(w) [list $var [dict keys $messages]]
    try {
	set Master ""
	while {$Master eq ""} {
	    set Master [yield]
	}
	lassign $Master message responder payload
	foreach vbl [dict get $bindings $message] value $payload {
	    upvar 1 $vbl v
	    set v $value
	}
	set body [dict get $messages $message]
	set code [uplevel 1 [list catch $body ::Select(em) ::Select(op)]]
	set opts $Select(op)
	if {$code == 1} {
	    dict append opts -errorinfo \
		"\n    while processing message\n$message $payload"
	}
	set $responder [list $code $Select(em) $opts]
    } finally {
	catch {unset $var}
	set Select(w) [lrange $Select(w) 0 end-1]
    }
}

# This acts as a receiver for messages, feeding them into the waiting
# [select].  It is incomplete as it should (but doesn't) queue messages that
# can't be received currently.
proc receive {message args} {
    global Select
    lassign [lindex $Select(w) end] var messages
    if {$message ni $messages} {
	throw BAD_MESSAGE "don't know message $message"
    }
    set responder [namespace which -variable Select](r[incr Select(c)])
    set $responder ""
    RTask [list $message $responder $args]
    set response [set $responder]
    unset responder
    after 1
    return $response
}

# This dispatches a message to a task in another thread.
proc send {target message args} {
    after 1
    set t [tsv::get tasks $target]
    if {![thread::send $t [list receive $message {*}$args] response]} {
	lassign $response code msg opts
	return -options $opts $msg
    } else {
	return -code error $response
    }
}
}]

# The backup printer task.
task BackupPrinter {
    set n 5
    while {$n >= 0} {
	select Print msg {
	    if {$n > 0} {
		incr n -1
		puts Backup:$msg
	    } else {
		throw OUT_OF_INK "out of ink"
	    }
	}
    }
}

# The main printer task.
task MainPrinter {
    set n 5
    set Backup BackupPrinter
    while 1 {
	select Print msg {
	    try {
		if {$n > 0} {
		    incr n -1
		    puts Main:$msg
		} elseif {$Backup ne ""} {
		    send $Backup Print $msg
		} else {
		    throw OUT_OF_INK "out of ink"
		}
	    } trap OUT_OF_INK {} {
		set Backup ""
		throw OUT_OF_INK "out of ink"
	    }
	}
    }
}

# Tasks that generate messages to print.
task HumptyDumpty {
    pause 100
    try {
	send MainPrinter Print "Humpty Dumpty sat on a wall."
	send MainPrinter Print "Humpty Dumpty had a great fall."
	send MainPrinter Print "All the King's horses and all the King's men"
	send MainPrinter Print "Couldn't put Humpty together again."
    } trap OUT_OF_INK {} {
	puts "Humpty Dumpty out of ink!"
    }
}
task MotherGoose {
    pause 100
    try {
	send MainPrinter Print "Old Mother Goose"
	send MainPrinter Print "When she wanted to wander,"
	send MainPrinter Print "Would ride through the air"
	send MainPrinter Print "On a very fine gander."
	send MainPrinter Print "Jack's mother came in,"
	send MainPrinter Print "And caught the goose soon,"
	send MainPrinter Print "And mounting its back,"
	send MainPrinter Print "Flew up to the moon."
    } trap OUT_OF_INK {} {
	puts "Mother Goose out of ink!"
    }
}

# Wait enough time for the example to run and then finish
after 1000
thread::broadcast thread::exit
```



## zkl

It is unfortunate the critical section is so long but there are several intertwined objects that can only be changed as a unit.

```zkl
class OutOfInk(Exception.IOError){
   const TEXT="Out of ink";
   text=TEXT;	// rename IOError to OutOfInk for this first/mother class
   fcn init{ IOError.init(TEXT) }  // this renames instances
}
class Printer{
   var id, ink;
   fcn init(_id,_ink){ id,ink=vm.arglist }
   fcn print(line){
      if(not ink) throw(OutOfInk);
      println("%s: %s".fmt(id,line));
      Atomic.sleep((0.0).random(0.01));  // don't let one thread dominate
      ink-=1;
   }
}
class RendezvousPrinter{  // the choke point between printers and tasks
   var printers=Thread.List();  // a thread safe list
   fcn init(_printers){ printers.extend(vm.arglist) }
   fcn print(line){  // caller waits for print job to finish
      var lines=Thread.List(); // fcn local [static] variable, the print queue
      lines.write(line);	// thread safe, stalls when full
      // lines is racy - other threads are modifing it, length is suspect here
      while(True){  // this thread can print that threads job
	 critical{  // creates a [global] mutex, automatically unlocks on exception
	    if(not printers) throw(OutOfInk); // No more printers to try
	    if(not lines) break; // only remove jobs in this serialized section
	    try{
	       printers[0].print(lines[0]);  // can throw
	       lines.del(0);  // successful print, remove job from queue
	    }catch(OutOfInk){ printers.del(0) } // Switch to the next printer
	 }
      }
   }
}
```


```zkl
fcn printTask(taskNm,rendezvous,lines){
   try{ foreach line in (vm.arglist[2,*]){ rendezvous.print(line); } }
   catch{ println(taskNm," caught ",__exception); } // and thread quits trying to print
}
fcn humptyDumptyTask(rendezvous){	// a thread
   printTask("humptyDumptyTask",rendezvous,
       "Humpty Dumpty sat on a wall.",
       "Humpty Dumpty had a great fall.",
       "All the king's horses and all the king's men,",
       "Couldn't put Humpty together again."
    )
}
fcn motherGooseTask(rendezvous){	// a thread
   printTask("motherGooseTask",rendezvous,
       "Old Mother Goose,",		"When she wanted to wander,",
       "Would ride through the air,",   "On a very fine gander.",
       "Jack's mother came in,",	"And caught the goose soon,",
       "And mounting its back,",	"Flew up to the moon."
   )
}
```


```zkl
rendezvous:=RendezvousPrinter(Printer("main",5), Printer("reserve",5));
humptyDumptyTask.launch(rendezvous);
motherGooseTask.launch(rendezvous);

Atomic.waitFor(fcn{ (not vm.numThreads) });  // wait for threads to finish
```

```txt

main: Humpty Dumpty sat on a wall.
main: Old Mother Goose,
main: Humpty Dumpty had a great fall.
main: All the king's horses and all the king's men,
main: When she wanted to wander,
reserve: Would ride through the air,
reserve: On a very fine gander.
reserve: Jack's mother came in,
reserve: And caught the goose soon,
reserve: And mounting its back,
motherGooseTask caught OutOfInk(Out of ink)
humptyDumptyTask caught OutOfInk(Out of ink)

```



{{omit from|E}} <!-- Not in the spirit of E concurrency; would be unenlightening to implement -->
{{omit from|JavaScript}} <!-- As yet, no multitasking unless you count setTimeout or multiple pages -->
{{omit from|Retro}} <!-- no concurrency in the standard implementations -->
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->

==See also==
* [[wp:Rendezvous|Rendezvous]] on Wikipedia.
