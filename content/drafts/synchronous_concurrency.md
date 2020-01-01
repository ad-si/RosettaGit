+++
title = "Synchronous concurrency"
description = ""
date = 2019-08-17T22:53:32Z
aliases = []
[extra]
id = 1930
[taxonomies]
categories = []
tags = []
+++

{{task|Concurrency}}{{requires|Concurrency}}{{omit from|BBC BASIC}}
The goal of this task is to create two concurrent activities ("[[Thread|Threads]]" or "Tasks", not [[Process|processes]].) that share data synchronously. Your language may provide syntax or libraries to perform concurrency. Different languages provide different implementations of concurrency, often with different names. Some languages use the term threads, others use the term tasks, while others use co-processes. This task should not be implemented using fork, spawn, or the [[Linux]]/[[UNIX]]/[[Windows|Win32]] pipe command, as communication should be between threads, not processes.

One of the concurrent units will read from a file named "input.txt" and send the contents of that file, one line at a time, to the other concurrent unit, which will print the line it receives to standard output. The printing unit must count the number of lines it prints. After the concurrent unit reading the file sends its last line to the printing unit, the reading unit will request the number of lines printed by the printing unit. The reading unit will then print the number of lines printed by the printing unit.

This task requires two-way communication between the concurrent units. All concurrent units must cleanly terminate at the end of the program.


## Ada

This Ada example starts by creating a package defining a single instance of a printing task. Ada requires packages to be separated into two parts. The package specification defines the interface to all public members of the package.

```Ada
package Synchronous_Concurrent is
   task Printer is
      entry Put(Item : in String);
      entry Get_Count(Count : out Natural);
   end Printer;
end Synchronous_Concurrent;
```

The package body contains the implementation of all the subprograms and tasks defined in the specification.

```Ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Synchronous_Concurrent is

   task body Printer is
      Num_Iter : Natural := 0;
      Line     : Unbounded_String;
   begin
      loop
         select
            accept Put(Item : in String) do
               Line := To_Unbounded_String(Item);
            end Put;
            Put_Line(To_String(Line));
            Num_Iter := Num_Iter + 1;
         or
            accept Get_Count(Count : out Natural) do
               Count := Num_Iter;
            end Get_Count;
         or terminate;
         end select;
      end loop;
   end Printer;

end Synchronous_Concurrent;
```

Note that the task body contains an ''accept'' block for each ''entry'' defined in the task specification. When some other task calls an entry in the Printer task the communication between the tasks is synchronized.

This example uses an infinite loop in the printer task. There is no way to know ahead of time how many lines the printer task will need to print. Each iteration through the loop causes the task to execute a selective ''accept''. That means that it can either ''accept'' a call on the Put entry, or it can ''accept'' a call on the Get_Count entry. The terminate option is execute only when the program contains no more tasks that can call the entries in the Printer task. If no task has called either entry the Printer task will wait for a task to call one of the entries, or for the terminate option to apply.

The next file contains the ''main'' procedure for this program. The main or entry-point procedure for a program always runs in the ''environment'' task. For this program, the ''environment'' task is takes on the role of the file reading concurrent unit while the Printer task takes on the role of the printing concurrent unit.

```Ada
with Synchronous_Concurrent; use Synchronous_Concurrent;
with Ada.Text_Io; use Ada.Text_Io;

procedure Synchronous_Concurrent_Main is
   Num_Strings : Natural;
   The_File : File_Type;
   Line : String(1..255);
   Length : Natural;
begin
   Open(File => The_File, Mode => In_File, Name => "input.txt");
   while not End_Of_File(The_File) loop
      Get_Line(File => The_File, Item => Line, Last => Length);
      Printer.Put(Line(1..Length));
   end loop;
   Close(The_File);
   Printer.Get_Count(Num_Strings);
   New_Line;
   Put_Line("The task wrote" & Natural'Image(Num_Strings) & " strings.");
end Synchronous_Concurrent_Main;
```

In this example only the environment task can call the entries on the Printer task. When the environment task completes the ''terminate'' option of the Printer task applies, terminating the Printer task. The environment task completes right after printing the number of lines sent through the Printer task. Because of the ''terminate'' option, the Printer task terminates just after the environment task prints the count.


## Aikido

Aikido supports threads and monitors natively (built in to the language).  There is also support for closures, but this example will use threads:

```aikido

monitor Queue {
    var items = []
    public function put (item) {
        items.append (item)
        notify()
    }

    public function get() {
        while (items.size() == 0) {
            wait()
        }
        var item = items[0]
        items <<= 1
        return item
    }

    public function close {
        items.append (none)
    }
}

thread reader (queue) {
    var numlines = 0
    for (;;) {
        var line = queue.get()
        if (typeof(line) == "none") {
            break
        }
        print (line)
        numlines++
    }
    println ("Number of lines: " + numlines)
}

thread writer (queue, lines) {
    foreach line lines {
        queue.put (line)
    }
    queue.close()
}

var queue = new Queue()
var lines = readfile ("input.txt")
var r = reader(queue)
var w = writer(queue, lines)

join (r)
join (w)
```



## ALGOL 68


```algol68
(
  STRING line;
  INT count := 0, errno;
  BOOL input complete := FALSE;
  SEMA output throttle = LEVEL 0, input throttle = LEVEL 1;

  FILE input txt;
  errno := open(input txt, "input.txt", stand in channel);

  PROC call back done = (REF FILE f) BOOL: ( input complete := TRUE );
  on logical file end(input txt, call back done);

  PAR (
    WHILE
      DOWN input throttle;
      get(input txt,(line, new line));
      UP output throttle;
      NOT input complete
    DO
      count+:=1
    OD
  ,
    WHILE
      DOWN output throttle;
      NOT input complete
    DO
      print((line, new line));
      UP input throttle
    OD
  );
  print((count))
)
```



## BCPL


```BCPL
// This is a BCPL implementation of the Rosettacode synchronous
// concurrency test using BCPL coroutines and a coroutine implementation
// of a Occum-style channels.
// BCPL is freely available from www.cl.cam.ac.uk/users/mr10

SECTION "coinout"

GET "libhdr.h"

GLOBAL {
  tracing: ug
}

LET start() = VALOF
{ LET argv = VEC 50
  LET in_co, out_co = 0, 0
  LET channel = 0
  LET filename = "input.txt"

  UNLESS rdargs("-f,-t/S", argv, 50) DO
  { writef("Bad arguments for coinout*n")
    RESULTIS 0
  }

  IF argv!0 DO filename := argv!0  // -f     the source file
  tracing := argv!1                // -t/S   tracing option

  in_co  := initco(infn,  500, @channel)
  out_co := initco(outfn, 500, @channel)

  UNLESS in_co & out_co DO
  { writef("Trouble creating the coroutines*n")
    GOTO fin
  }

  IF tracing DO writef("*nBoth in and out coroutines created*n*n")

  callco(in_co, filename)

fin:
  IF in_co  DO deleteco(in_co)
  IF out_co DO deleteco(out_co)

  IF tracing DO writef("Both in and out coroutines deleted*n*n")

  RESULTIS 0
}

AND readline(line) = VALOF
{ LET ch, i = 0, 0
  line%0 := 0

  { ch := rdch()
    IF ch=endstreamch RESULTIS FALSE
    i := i+1
    line%0, line%i := i, ch
    IF ch='*n' | i=255 BREAK
  } REPEAT

  RESULTIS TRUE
}

AND infn(args) BE
{ LET channelptr = args!0
  LET name = cowait()  // Get the file name
  LET instream = findinput(name)
  LET line = VEC 256/bytesperword

  UNLESS instream DO
  { writef("*nTrouble with file: %s*n", name)
    RETURN
  }

  selectinput(instream)

  { LET ok = readline(line)
    UNLESS ok BREAK
    IF tracing DO
      writef("inco:  Sending a line to outco*n")
    cowrite(channelptr, line)
  } REPEAT

  IF tracing DO
    writef("inco:  Sending zero to outco*n")

  writef("*nNumber of lines written was %n*n", cowrite(channelptr, 0))

  endstream(instream)
}

AND outfn(args) BE
{ LET channelptr = args!0
  LET linecount = 0

  { LET line = coread(channelptr)
    UNLESS line BREAK
    IF tracing DO writef("outfn: Received a line*n")
    writes(line)
    linecount := linecount + 1
  } REPEAT

  IF tracing DO
    writef("outfn: Received zero, so sent count=%n back to inco*n",
            linecount)

  cowait(linecount)
}

// The following functions are a  implementation of Occum-style channels
// using coroutines.

// The first coroutine to request a transfer through a channel becomes
// suspended and the second causes the data to be transfers and then allows
// both coroutines to resume (in some order). The channel word is either
// zero or points to a suspended (read or write) cocoutine.

// The use of resumeco in coread is somewhat subtle!

AND coread(ptr) = VALOF
{ LET cptr = !ptr
  TEST cptr
  THEN { !ptr := 0         // Clear the channel word
         RESULTIS resumeco(cptr, currco)
       }
  ELSE { !ptr := currco    // Set channel word to this coroutine
         RESULTIS cowait() // Wait for value from cowrite
       }
}

AND cowrite(ptr, val) BE
{ LET cptr = !ptr
  TEST cptr
  THEN { !ptr := 0
         callco(cptr, val) // Send val to coread
       }
  ELSE { !ptr := currco
          callco(cowait(), val)
       }
}

```



## C

{{libheader|libco}}

```cpp
#include <iostream>	/* malloc(), realloc(), free() */
#include <stdio.h>	/* fopen(), fgetc(), fwrite(), printf() */

#include <libco.h>	/* co_create(), co_switch() */

void
fail(const char *message) {
	perror(message);
	exit(1);
}

/*
 * These are global variables of this process. All cothreads of this
 * process will share these global variables.
 */
cothread_t reader;
cothread_t printer;
struct {
	char	*buf;	/* Not a C string. No terminating '\0'. */
	size_t	len;	/* Length of line in buffer. */
	size_t	cap;	/* Maximum capacity of buffer. */
} line;
size_t count;		/* Number of lines printed. */

/*
 * The reader cothread reads every line of an input file, passes each
 * line to the printer cothread, and reports the number of lines.
 */
void
reader_entry(void)
{
	FILE *input;
	size_t newcap;
	int c, eof, eol;
	char *newbuf;

	input = fopen("input.txt", "r");
	if (input == NULL)
		fail("fopen");

	line.buf = malloc(line.cap = 4096);  /* New buffer. */
	if (line.buf == NULL)
		fail("malloc");
	line.len = 0;  /* Start with an empty line. */

	do {
		c = fgetc(input);  /* Read next character. */
		if (ferror(input))
			fail("fgetc");

		eof = (c == EOF);
		if (eof) {
			/*
			 * End of file is also end of line,
		`	 * unless the line would be empty.
			 */
			eol = (line.len > 0);
		} else {
			/* Append c to the buffer. */
			if (line.len == line.cap) {
				/* Need a bigger buffer! */
				newcap = line.cap * 2;
				newbuf = realloc(line.buf, newcap);
				if (newbuf == NULL)
					fail("realloc");
				line.buf = newbuf;
				line.cap = newcap;
			}
			line.buf[line.len++] = c;

			/* '\n' is end of line. */
			eol = (c == '\n');
		}

		if (eol) {
			/* Pass our complete line to the printer. */
			co_switch(printer);
			line.len = 0;  /* Destroy our line. */
		}
	} while (!eof);

	free(line.buf);
	line.buf = NULL;  /* Stops a loop in the printer. */

	printf("Printed %zu lines.\n", count);
	co_switch(printer);
}

/*
 * The printer cothread starts the reader cothread, prints every line
 * line from the reader cothread, and counts the number of lines.
 */
int
main()
{
	reader = co_create(4096, reader_entry);
	printer = co_active();
	count = 0;

	for (;;) {
		co_switch(reader);
		if (line.buf == NULL)
			break;

		/* Print this line. Count it. */
		fwrite(line.buf, 1, line.len, stdout);
		count++;
	}

	co_delete(reader);
	return 0;
}
```



## C++

{{works with|C++11}}

```cpp
#include <future>
#include <iostream>
#include <fstream>
#include <mutex>
#include <queue>
#include <string>
#include <thread>

struct lock_queue
{
    std::queue<std::string> q;
    std::mutex mutex;
};

void reader(std::string filename, std::future<size_t> lines, lock_queue& out)
{
    std::string line;
    std::ifstream in(filename);
    while(std::getline(in, line)) {
        line += '\n';
        std::lock_guard<std::mutex> lock(out.mutex);
        out.q.push(line);
    } {
        std::lock_guard<std::mutex> lock(out.mutex);
        out.q.push("");
    }
    lines.wait();
    std::cout << "\nPrinted " << lines.get() << " lines\n";
}

void printer(std::promise<size_t> lines, lock_queue& in)
{
    std::string s;
    size_t line_n = 0;
    bool print = false;
    while(true) {
        {
            std::lock_guard<std::mutex> lock(in.mutex);
            if(( print = not in.q.empty() )) { //Assignment intended
                s = in.q.front();
                in.q.pop();
            }
        }
        if(print) {
            if(s == "") break;
            std::cout << s;
            ++line_n;
            print = false;
        }
    }
    lines.set_value(line_n);
}

int main()
{
    lock_queue queue;
    std::promise<size_t> promise;
    std::thread t1(reader, "input.txt", promise.get_future(), std::ref(queue));
    std::thread t2(printer, std::move(promise), std::ref(queue));
    t1.join(); t2.join();
}
```

{{out}}

```txt

Lorem ipsum dolor sit amet, consectetur adipisicing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.

Printed 9 lines

```



## C sharp


```csharp
using System;
using System.Threading.Tasks;
using System.Collections.Concurrent;
using System.IO;

namespace SynchronousConcurrency
{
    class Program
    {
        static void Main(string[] args)
        {
            BlockingCollection<string> toWriterTask = new BlockingCollection<string>();
            BlockingCollection<int> fromWriterTask = new BlockingCollection<int>();
            Task writer = Task.Factory.StartNew(() => ConsoleWriter(toWriterTask, fromWriterTask));
            Task reader = Task.Factory.StartNew(() => FileReader(fromWriterTask, toWriterTask));
            Task.WaitAll(writer, reader);
        }
        static void ConsoleWriter(BlockingCollection<string> input, BlockingCollection<int> output)
        {
            int nLines = 0;
            string line;
            while ((line = input.Take()) != null)
            {
                Console.WriteLine(line);
                ++nLines;
            }
            output.Add(nLines);
        }
        static void FileReader(BlockingCollection<int> input, BlockingCollection<string> output)
        {
            StreamReader file = new StreamReader("input.txt"); // TODO: check exceptions
            string line;
            while ((line = file.ReadLine()) != null)
            {
                output.Add(line);

            }
            output.Add(null); // EOF
            Console.WriteLine("line count: " + input.Take());
        }
    }
}
```

{{out}}

```txt

foo
bar
baz
xenu 666
line count: 4

```



## Clojure

The writer executes as an agent on a thread from a thread pool. The state of the agent is the count of written lines,
initialized to 0. The reader will send the writer calls to the ''write-line'' function, which the agent will execute asynchronously.
The ''state'' argument is the agent's state at the start of the call, and the last expression becomes the agent's new state.


```clojure
(use '[clojure.java.io :as io])

(def writer (agent 0))

(defn write-line [state line]
  (println line)
  (inc state))
```


The reader executes on the main thread. It passes each line to the writer -- the ''send'' call returns immediately, waits until the writer has finished writing all the lines, gets the line count & prints it, and terminates the writer.


```clojure
(with-open [r (io/reader "input.txt")]
  (doseq [line (line-seq r)]
    (send writer write-line line)))
(await writer)
(println "lines written:" @writer)
(shutdown-agents)
```

That's it!


## Common Lisp


{{libheader|Bordeaux Threads}}

First, implement message-passing:


```lisp
(defvar *self*)

(defclass queue ()
  ((condition :initform (make-condition-variable)
              :reader condition-of)
   (mailbox :initform '()
            :accessor mailbox-of)
   (lock :initform (make-lock)
         :reader lock-of)))

(defun message (recipient name &rest message)
  (with-lock-held ((lock-of recipient))
    ;; it would have been better to implement tail-consing or a LIFO
    (setf (mailbox-of recipient)
          (nconc (mailbox-of recipient)
                 (list (list* name message))))
    (condition-notify (condition-of recipient)))
  message)

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun slurp-message ()
  (with-lock-held ((lock-of *self*))
    (if (not (endp (mailbox-of *self*)))
        (pop (mailbox-of *self*))
        (progn (condition-wait (condition-of *self*)
                               (lock-of *self*))
               (assert (not (endp (mailbox-of *self*))))
               (pop (mailbox-of *self*))))))

(defmacro receive-message (&body cases)
  (let ((msg-name (gensym "MESSAGE"))
        (block-name (gensym "BLOCK")))
    `(let ((,msg-name (slurp-message)))
       (block ,block-name
         ,@(loop for i in cases
                 for ((name . case) . body) = (cons (mklist (car i))
                                                    (cdr i))
                 when (typep i '(or (cons (eql quote)
                                          t)
                                    (cons (cons (eql quote) t)
                                          t)))
                   do (warn "~S is a quoted form" i)
                 collect `(when ,(if (null name)
                                     't
                                     `(eql ',name (car ,msg-name)))
                            (destructuring-bind ,case
                                (cdr ,msg-name)
                              (return-from ,block-name
                                (progn ,@body)))))
         (error "Unknown message: ~S" ,msg-name)))))

(defmacro receive-one-message (message &body body)
  `(receive-message (,message . ,body)))

(defun queue () (make-instance 'queue))
```


Should be easy from now on:


```lisp
(defun reader (pathname writer)
  (with-open-file (stream pathname)
    (loop for line = (read-line stream nil)
          while line
          do (message writer '|here's a line for you| line)
          finally
       (message writer '|how many lines?|)
       (receive-one-message (|line count| count)
          (format t "line count: ~D~%" count))
       (message writer '|looks like i've got no more lines|))))

(defun writer (stream reader)
  ;; that would work better with ITERATE
  (loop with line-count = 0 do
    (receive-message
     ((|here's a line for you| line)
      (write-line line stream)
      (incf line-count))
     (|looks like i've got no more lines|
      (return))
     (|how many lines?|
      (message reader '|line count| line-count)))))

(defmacro thread (queue &body body)
  `(make-thread (lambda (&aux (*self* ,queue))
                  ,@body)))

(defun synchronous-concurrency (&key (pathname "input.txt"))
  (let ((reader (queue))
        (writer (queue)))
    (thread reader (reader pathname writer))
    (thread writer (writer *standard-output* reader)))
  (values))
```


And now an example:


```lisp
CL-USER> (synchronous-concurrency :pathname "/tmp/input.txt")
foo
bar
baz
xenu 666
line count: 4
; No value
```


Note that to run the example from the SLIME REPL you need to put:

  (setq swank:*globally-redirect-io* t)

in your <code>~/.swank.lisp</code>


## D


```d
import std.algorithm, std.concurrency, std.stdio;

void main() {
    auto printer = spawn(&printTask, thisTid);
    auto f = File("input.txt","r");
    foreach (string line; lines(f))
        send(printer, line);
    send(printer, true);    //EOF
    auto n = receiveOnly!(int)();
    stdout.writefln("\n%d lines printed.", n);
}

void printTask(Tid reader) {
    int n = 0;
    for (bool eof = false; !eof;)
        receive(
            (string line) {stdout.write(line); n++;},
            (bool) {send(reader, n); eof = true;}
        );
}

```



## Delphi


```Delphi

program Project2;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows;

type
  EThreadStackFinalized = class(Exception);

  PLine = ^TLine;
  TLine = record
    Text: string;
  end;

  TThreadQueue = class
  private
    FFinalized: Boolean;
    FQueue: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Finalize;
    procedure Push(Data: Pointer);
    function Pop(var Data: Pointer): Boolean;
    property Finalized: Boolean read FFinalized;
  end;

  TPrintThread = class(TThread)
  private
    FCount: Integer;
    FTreminateEvent: THandle;
    FDoneEvent: THandle;
    FQueue: TThreadQueue;
  public
    constructor Create(aTreminateEvent, aDoneEvent: THandle; aQueue: TThreadQueue);
    procedure Execute; override;

    property Count: Integer read FCount;
  end;

{ TThreadQueue }

constructor TThreadQueue.Create;
begin
  FQueue := CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  FFinalized := False;
end;

destructor TThreadQueue.Destroy;
begin
  if FQueue <> 0 then
    CloseHandle(FQueue);
  inherited;
end;

procedure TThreadQueue.Finalize;
begin
  PostQueuedCompletionStatus(FQueue, 0, 0, Pointer($FFFFFFFF));
  FFinalized := True;
end;

function TThreadQueue.Pop(var Data: Pointer): Boolean;
var
  A: Cardinal;
  OL: POverLapped;
begin
  Result := True;
  if not FFinalized then
    GetQueuedCompletionStatus(FQueue, A, Cardinal(Data), OL, INFINITE);

  if FFinalized or (OL = Pointer($FFFFFFFF)) then begin
    Data := nil;
    Result := False;
    Finalize;
  end;
end;

procedure TThreadQueue.Push(Data: Pointer);
begin
  if FFinalized then
    raise EThreadStackFinalized.Create('Stack is finalized');

  PostQueuedCompletionStatus(FQueue, 0, Cardinal(Data), nil);
end;

{ TPrintThread }

constructor TPrintThread.Create(aTreminateEvent, aDoneEvent: THandle; aQueue: TThreadQueue);
begin
  inherited Create(True);
  FCount := 0;
  FreeOnTerminate := True;
  FTreminateEvent := aTreminateEvent;
  FDoneEvent := aDoneEvent;
  FQueue := aQueue;
end;

procedure TPrintThread.Execute;
var
  data: Pointer;
  line: PLine;
begin
  repeat
    if FQueue.Pop(data) then begin
      line := data;
      try
        Writeln(line^.Text);
        if line^.Text = #0 then
          SetEvent(FDoneEvent);
        Inc(FCount);
      finally
        Dispose(line);
      end;
    end;

  until False;
  WaitForSingleObject(FTreminateEvent, INFINITE);
end;

var
  PrintThread: TPrintThread;
  Queue: TThreadQueue;
  lines: TStrings;
  i: Integer;
  line: PLine;
  TreminateEvent, DoneEvent: THandle;
begin
  Queue := TThreadQueue.Create;
  try
    TreminateEvent := CreateEvent(nil, False, False, 'TERMINATE_EVENT');
    DoneEvent := CreateEvent(nil, False, False, 'DONE_EVENT');
    try
      PrintThread := TPrintThread.Create(TreminateEvent, DoneEvent, Queue);
      PrintThread.Start;
      lines := TStringList.Create;
      try
        lines.LoadFromFile('input.txt');
        for i := 0 to lines.Count - 1 do begin
          New(line);
          line^.Text := lines[i];
          Queue.Push(line);
        end;

        New(line);
        line^.Text := #0;
        Queue.Push(line);

        WaitForSingleObject(DoneEvent, INFINITE);

        New(line);
        line^.Text := IntToStr(PrintThread.Count);
        Queue.Push(line);

        SetEvent(TreminateEvent);
      finally
        lines.Free;
      end;
    finally
      CloseHandle(TreminateEvent);
      CloseHandle(DoneEvent)
    end;

    Readln;
  finally
    Queue.Free;
  end;
end.

```



## E


```e
def printer := {
    var count := 0
    def printer {
        to run(item) {
            count += 1
            println(item)
        }
        to getCount() {
            return count
        }
    }
}

def sender(lines) {
    switch (lines) {
        match [] {
            when (def count := printer <- getCount()) -> {
                println(`$count lines were printed.`)
            }
        }
        match [line] + rest {
            when (printer <- run(line)) -> {
                sender(rest)
            }
        }
    }
}

# Stream IO in E is not finished yet, so this example just uses a list.
sender(<file:input.txt>.getText().split("\n"))
```



## EchoLisp

This task uses two processes, reader/writer, synchronized by a semaphore S and its queue of messages. The reader sends write or reader-count-please messages. The writer responds with ack or count messages.

```scheme

(require 'sequences)
(require 'tasks)

;; inter-tasks message : (op-code . data)
(define (is-message? op message)
	(and message (equal? op (first message))))

;; reader task
(define (reader infile )
	(wait S)
	(define message (semaphore-pop S))
	(when (is-message? 'count message ) (writeln 'reader-> message) (task-stop-all))

	(if (first infile) ;; not EOF
	(set! message (cons 'write (next infile)))
	(set! message (list 'reader-count-please)))

	(semaphore-push S message)
	(signal S)
	infile)

(define (writer count)
	(wait S)
	(define message (semaphore-pop S))
	(when  (is-message? 'write message )
			(writeln (rest message))
			(set! count (1+ count))
			(set! message (cons 'ack count)))

	(when  (is-message? 'reader-count-please message )
		 (set! message (cons 'count count)))
	(semaphore-push S message)
	(signal S)
	count)


```

{{out}}

```txt

;; input sequence
(define infile null) ;; input file handle
(define (set-infile name text) (set! infile (procrastinator (string-split text "\n"))))
(file->string set-infile "input.txt")

(define S (make-semaphore 1)) ;; semaphore to synchronize
(task-run (make-task reader infile))
(task-run (make-task writer 0))

    #task:id:165:running
    #task:id:166:running
    Well, it's one for the money
    Two for the show
    Three to get ready
    Now go, cat, go
    But don't you
    Step on my blue suede shoes
    You can do anything
    But stay off of my blue suede shoes
    Well, you can knock me down
 [lines skipped ...]
    Blue, suede shoes, baby
    Blue, blue
    Blue suede shoes
    Well, you can do anything
    But stay off of my blue suede shoes!

    reader->     (count . 53)

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def start do
     my_pid = self
     pid = spawn( fn -> reader(my_pid, 0) end )
     File.open( "input.txt", [:read], fn io ->
       process( IO.gets(io, ""), io, pid )
     end )
  end

  defp process( :eof, _io, pid ) do
    send( pid, :count )
    receive do
      i -> IO.puts "Count:#{i}"
    end
  end
  defp process( any, io, pid ) do
    send( pid, any )
    process( IO.gets(io, ""), io, pid )
  end

  defp reader( pid, c ) do
    receive do
      :count -> send( pid, c )
      any ->
        IO.write any
        reader( pid, c+1 )
    end
  end
end

RC.start
```



## Erlang



```erlang
-module(cc).

-export([start/0]).

start() ->
   My_pid = erlang:self(),
   Pid = erlang:spawn( fun() -> reader(My_pid, 0) end ),
   {ok, IO } = file:open( "input.txt", [read] ),
   process( io:get_line(IO, ""), IO, Pid ),
   file:close( IO ).

process( eof, _IO, Pid ) ->
   Pid ! count,
   receive
       I -> io:fwrite("Count:~p~n", [I])
   end;
process( Any, IO, Pid ) ->
   Pid ! Any,
   process( io:get_line(IO, ""), IO, Pid ).

reader(Pid, C) ->
   receive
       count -> Pid ! C;
       Any ->
           io:fwrite("~s", [Any]),
           reader(Pid, C+1)
   end.
```



## Euphoria


```euphoria
sequence lines
sequence count
lines = {}
count = {}

procedure read(integer fn)
    object line
    while 1 do
        line = gets(fn)
        if atom(line) then
            exit
        else
            lines = append(lines, line)
            task_yield()
        end if
    end while

    lines = append(lines,0)
    while length(count) = 0 do
        task_yield()
    end while

    printf(1,"Count: %d\n",count[1])
end procedure

procedure write(integer fn)
    integer n
    object line
    n = 0
    while 1 do
        while length(lines) = 0 do
            task_yield()
        end while

        line = lines[1]
        lines = lines[2..$]
        if atom(line) then
            exit
        else
            puts(fn,line)
            n += 1
        end if
    end while
    count = append(count,n)
end procedure

integer fn
atom reader, writer
constant stdout = 1
fn = open("input.txt","r")
reader = task_create(routine_id("read"),{fn})
writer = task_create(routine_id("write"),{stdout})
task_schedule(writer,1)
task_schedule(reader,1)

while length(task_list()) > 1 do
    task_yield()
end while
```



## Forth

Tested in GForth 0.7.3. First, 'co.fs' - the coroutines library, then the output of the application on himself,
result of the command-line: 'cat synco.fs | gforth synco.fs'.

```forth
\
\ co.fs		Coroutines by continuations.
\
\ * Circular Queue. Capacity is power of 2.
\
	VARIABLE HEAD VARIABLE TAIL
	128 CELLS CONSTANT CQ#
\ * align by queue capacity
	HERE DUP
		CQ# 1- INVERT AND CQ# +
	SWAP - ALLOT
\
	HERE CQ# ALLOT CONSTANT START
\
: ADJUST   (  -- )   [ CQ# 1- ]L AND START + ;
: PUT      ( n-- )   TAIL @ TUCK ! CELL+ ADJUST TAIL ! ;
: TAKE 	   ( --n )   HEAD @ DUP @ SWAP CELL+ ADJUST HEAD ! ;
: 0CQ	   ( --  )   START DUP HEAD ! TAIL ! ; 0CQ
: NOEMPTY? ( --f )   HEAD @ TAIL @ <> ;
: ;CO      ( --  )   TAKE >R ;
\
\ * COROUTINES LEXEME
\
: CO:  ( -- )   R>  PUT ;	  \ Register continuation as coroutine. Exit.
: CO   ( -- )   R>  PUT TAKE >R ; \ Co-route.
: GO   ( -- )   BEGIN NOEMPTY? WHILE ;CO REPEAT ; \ :-)
\
\ * CHANNELS LEXEME
\
: CHAN?  ( a--f )   2@ XOR ;

```

{{out}}

```txt

\ synco.fs	Synchronous concurrency for RosettaCode
include co.fs

: STRING@	DUP CELL + SWAP @ ;

	2VARIABLE CHAN

\
\ * READER LEXEME
\
	4096 CONSTANT L#
	CREATE Line 0 , L# ALLOT
: READER
	CO:
	BEGIN
		Line cell+ L# STDIN read-line THROW
	WHILE
		Line !
		0 Line CHAN 2!
		CO
	REPEAT	DROP
	Line DUP CHAN 2!

	\ -- Wait for report back
	BEGIN CO CHAN CHAN? UNTIL

	\ -- Have it, show and go
	CR S" -------" TYPE
	CR S" LINES: " TYPE CHAN @ ?
;
\
\ * WRITER LEXEME
\
	VARIABLE X
: WRITER
	CO:
	BEGIN
		CHAN CHAN?
	WHILE
		CHAN @ STRING@ TYPE CR
		1 X +!
		CO
	REPEAT

	\ -- Chance to stop other writers
	CO

	\ -- First of writers reports back
	\ -- the shared global counter
	CHAN CHAN? 0=
	IF
		0 X CHAN 2!
		CO
	THEN
;
\
\ * RUNNER
\
0 X ! READER WRITER ( WRITER WRITER :-) GO CR BYE

-------
LINES: 59

```


=={{header|F_Sharp|F#}}==

This code will read lines from the file on one thread, and print them to the console on one or more other
threads from the ThreadPool, using a MailboxProcessor for lock-free communication between threads and tracking the line count without the use of mutable state.


```fsharp

open System.IO

type Msg =
    | PrintLine of string
    | GetCount of AsyncReplyChannel<int>

let printer =
    MailboxProcessor.Start(fun inbox ->
        let rec loop count =
            async {
                let! msg = inbox.Receive()
                match msg with
                | PrintLine(s) ->
                    printfn "%s" s
                    return! loop (count + 1)
                | GetCount(reply) ->
                    reply.Reply(count)
                    return! loop count
            }
        loop 0
    )

let reader (printAgent:MailboxProcessor<Msg>) file =
    File.ReadLines(file)
    |> Seq.iter (fun line -> PrintLine line |> printAgent.Post)
    printAgent.PostAndReply(fun reply -> GetCount(reply))
    |> printfn "Lines written: %i"

reader printer @"c:\temp\input.txt"

```



## Go


```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    lines := make(chan string)
    count := make(chan int)
    go func() {
        c := 0
        for l := range lines {
            fmt.Println(l)
            c++
        }
        count <- c
    }()

    f, err := os.Open("input.txt")
    if err != nil {
        log.Fatal(err)
    }
    for s := bufio.NewScanner(f); s.Scan(); {
        lines <- s.Text()
    }
    f.Close()
    close(lines)
    fmt.Println("Number of lines:", <-count)
}
```



## Haskell

The following Haskell code uses simple MVars for thread communication. While the GHC libraries for concurrency give quite a wide design space for thread communication, I felt that the following was fairly reasonable.

For those who are unaware of MVars, they are essentially mutable cells which may be empty or hold a single value, and which have the following important properties:

* takeMVar will get the contents of an MVar when it is full, emptying it.
* takeMVar will block if the MVar is empty, until it has been filled by another thread.
* putMVar will fill an empty MVar with a given value.
* putMVar will block until the MVar is empty if it is full.

So MVars are essentially bounded channels which hold a maximum of one element at a time.

The code below defines various signals in terms of takeMVar and putMVar and then passes those to the parts of the code which should be permitted to use them. Note that this way, it is impossible for the reader process to take the current line, for example.


```haskell
import Control.Concurrent
import Control.Concurrent.MVar

main =
    do lineVar <- newEmptyMVar
       countVar <- newEmptyMVar

       let takeLine  = takeMVar lineVar
           putLine   = putMVar lineVar . Just
           putEOF    = putMVar lineVar Nothing
           takeCount = takeMVar countVar
           putCount  = putMVar countVar

       forkIO $ writer takeLine putCount
       reader putLine putEOF takeCount
```


The reader simply reads the file lazily, applying putLine to each of the lines in turn, which blocks until the writer has taken the line. It then signals that it is finished with putEOF, and then takes the count and prints it.


```haskell
reader putLine putEOF takeCount =
    do ls <- fmap lines (readFile "input.txt")
       mapM putLine ls
       putEOF
       n <- takeCount
       print n
```


The writer gets the lines in a loop with takeLine until it receives Nothing, at which point it uses putCount to tell the reader how many lines there were.


```haskell
writer takeLine putCount = loop 0
  where loop n = do l <- takeLine
                    case l of
                       Just x  -> do putStrLn x
                                     loop (n+1)
                       Nothing -> putCount n
```


=={{header|Icon}} and {{header|Unicon}}==

Icon does not support concurrent programming.  Co-expressions can be used in
Icon to mimic some of the behavior, but they are not truly concurrently
executed.

Unicon supports concurrent programming.  A Unicon solution is:


```unicon
procedure main(A)
    fName := A[1]|"index.txt"
    p := thread produce(fName)
    c := thread consume(p)
    every wait(p | c)
end

procedure produce(fName)
    every !open(fName)@>>    # drop messages in p's outbox (blocking whenever box is full)
    @>>                      # Signal consumer that p is done
    write("count is ",<<@)   # block until message in p's inbox
end

procedure consume(p)
    i := 0
    while write(\<<@p) do (i+:=1)  # loop until empty message in p's outbox
                                   #    (blocking until each message arrives)
    i@>>p                          # put row count into p's inbox
end
```


Sample output:

```txt

->pc
procedure main()
   n := 5134
   x := *n
   write(x)
end
count is 5
->

```



## J


Implementation:

```J
input=: 1 :0
  nlines=: 0
  u;._2@fread 'input.txt'
  smoutput nlines
)

output=: 3 :0
  nlines=: nlines+1
  smoutput y
)
```


Usage:


```J>   output input</lang


Note that we are not using OS threads here, but instead - as specified by this task - are using two concurrent activities which share data synchronously.


## Java



```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

class SynchronousConcurrency
{
  public static void main(String[] args) throws Exception
  {
    final AtomicLong lineCount = new AtomicLong(0);
    final BlockingQueue<String> queue = new LinkedBlockingQueue<String>();
    final String EOF = new String();

    final Thread writerThread = new Thread(new Runnable() {
        public void run()
        {
          long linesWrote = 0;
          while (true)
          {
            try
            {
              String line = queue.take();
              // Reference equality
              if (line == EOF)
                break;
              System.out.println(line);
              linesWrote++;
            }
            catch (InterruptedException ie)
            {  }
          }
          lineCount.set(linesWrote);
        }
      }
    );
    writerThread.start();

    // No need to start a third thread for the reader, just use this thread
    BufferedReader br = new BufferedReader(new FileReader("input.txt"));
    String line;
    while ((line = br.readLine()) != null)
      queue.put(line);
    br.close();
    queue.put(EOF);
    writerThread.join();
    // AtomicLong is not needed here due to memory barrier created by thread join, but still need a mutable long since lineCount must be final to access it from an anonymous class
    System.out.println("Line count: " + lineCount.get());
    return;
  }
}

```



## Julia


```julia

function inputlines(txtfile, iochannel)
    for line in readlines(txtfile)
        Base.put!(iochannel, line)
    end
    Base.put!(iochannel, nothing)
    println("The other task printed $(take!(iochannel)) lines.")
end

function outputlines(iochannel)
    totallines = 0
    while (line = Base.take!(iochannel)) != nothing
        totallines += 1
        println(line)
    end
    Base.put!(iochannel, totallines)
end

c = Channel(0)
@async inputlines("filename.txt", c)
outputlines(c)

```



## Kotlin



### Using threads



```scala
// version 1.1.51

import java.util.concurrent.SynchronousQueue
import kotlin.concurrent.thread
import java.io.File

val queue = SynchronousQueue<String>()

const val EOT = "\u0004"  // end of transmission

fun main(args: Array<String>) {
    thread {
        var count = 0

        while (true) {
             val line = queue.take()
             if (line == EOT) {
                queue.put(count.toString())
                break
             }
             println(line)
             count++
        }
    }

    File("input.txt").forEachLine { line -> queue.put(line) }
    queue.put(EOT)
    val count = queue.take().toInt()
    println("\nNumber of lines printed = $count")
}
```


Content of input.txt:

```txt

line1
line2
line3
line4

```


{{out}}

```txt

line1
line2
line3
line4

Number of lines printed = 4

```



### Using coroutines

Uses experimental features and will change in the future. Same output as the threads version.

```scala

// version 1.3.20 with kotlinx-coroutines-core version 1.1.1

import kotlinx.coroutines.async
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.sumBy
import kotlinx.coroutines.coroutineScope
import java.io.File

suspend fun main() {
    coroutineScope {
        val lines = Channel<String>()

        val count = async {
            lines.sumBy { line ->
                println(line)
                1
            }
        }

        File("input.txt").bufferedReader().useLines { text ->
	    for (line in text) {
                lines.send(line)
            }
        }
        lines.close()
        println("\nNumber of lines printed = ${count.await()}")
    }
}

```



## Logtalk

The following example can be found in the Logtalk distribution and is used here with permission. The "input.txt" file contains the terms "a(0)..a(9)", one per line. Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk

:- object(team).

    :- threaded.

    :- public(start/0).
    start :-
        threaded((
            reader,
            writer(0)
        )).

    reader :-
        open('input.txt', read, Stream),
        repeat,
            read_term(Stream, Term, []),
            threaded_notify(term(Term)),
        Term == end_of_file,
        !,
        close(Stream),
        threaded_wait(lines(Lines)),
        write('Number of lines: '), write(Lines), nl.

    writer(N0) :-
        threaded_wait(term(Term)),
        (   Term == end_of_file ->
            threaded_notify(lines(N0))
        ;   N is N0 + 1,
            write(Term), nl,
            writer(N)
        ).

:- end_object.

```


Output:


```text

| ?- ?- team::start.
a(0)
a(1)
a(2)
a(3)
a(4)
a(5)
a(6)
a(7)
a(8)
a(9)
Number of lines: 10
true.

```



## Lua


```lua
function ReadFile()
    local fp = io.open( "input.txt" )
    assert( fp ~= nil )

    for line in fp:lines() do
	coroutine.yield( line )
    end

    fp:close()
end

co = coroutine.create( ReadFile )

while true do
    local status, val = coroutine.resume( co )
    if coroutine.status( co ) == "dead" then break end
    print( val )
end

```



## Mercury


```Mercury
:- module synchronous_concurrency.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int, list, string, thread, thread.channel, thread.mvar.

:- type line_or_stop
    --->    line(string)
    ;       stop.

main(!IO) :-
    io.open_input("input.txt", Res, !IO),
    (
        Res = ok(Input),
        channel.init(Channel, !IO),
        mvar.init(MVar, !IO),
        thread.spawn(writer(Channel, MVar, 0), !IO),
        reader(Input, Channel, MVar, !IO)
    ;
        Res = error(Err),
        io.format("Error opening file: %s\n", [s(io.error_message(Err))], !IO)
    ).

:- pred reader(io.text_input_stream::in, channel(line_or_stop)::in, mvar(int)::in,
    io::di, io::uo) is det.

reader(Input, Channel, MVar, !IO) :-
    io.read_line_as_string(Input, Res, !IO),
    (
        Res = ok(Line),
        channel.put(Channel, line(Line), !IO),
        reader(Input, Channel, MVar, !IO)
    ;
        Res = eof,
        channel.put(Channel, stop, !IO),
        mvar.take(MVar, Count, !IO),
        io.format("%d lines printed.\n", [i(Count)], !IO)
    ;
        Res = error(Err),
        channel.put(Channel, stop, !IO),
        io.format("Error reading file: %s\n", [s(io.error_message(Err))], !IO)
    ).

:- pred writer(channel(line_or_stop)::in, mvar(int)::in, int::in,
    io::di, io::uo) is cc_multi.

writer(Channel, MVar, Count, !IO) :-
    channel.take(Channel, LineOrStop, !IO),
    (
        LineOrStop = line(Line),
        io.write_string(Line, !IO),
        writer(Channel, MVar, Count + 1, !IO)
    ;
        LineOrStop = stop,
        mvar.put(MVar, Count, !IO)
    ).
```



## Nim

Compile with <code>nim --threads:on c sync.nim</code>:

{{trans|Python}}
Two threads are spun up that communicate over a channel.


```nim
var msgs: Channel[string]
var count: Channel[int]

const FILE = "input.txt"

proc read() {.thread.} =
  var file = open(FILE)
  for line in file.lines:
    msgs.send(line)
  msgs.send(nil)
  file.close()
  echo count.recv()
  count.close()

proc print() {.thread.} =
  var n = 0
  while true:
    var msg = msgs.recv()
    if msg == nil:
      break
    echo msg
    n += 1
  msgs.close()
  count.send(n)

var reader_thread = Thread[void]()
var printer_thread = Thread[void]()

msgs.open()
count.open()
createThread(reader_thread, read)
createThread(printer_thread, print)
joinThreads(reader_thread, printer_thread)
```



## OCaml


### Using only the standard library

We use the built-in Event module to provide communication channels between threads.

```ocaml>open Event</lang


The reader is a simple loop. It starts by opening a file, then reads lines from that file until there is nothing left to read. After each line, it sends <tt>Some v</tt> on channel <tt>lines_dest</tt>, where <tt>v</tt> is the contents of the line. Once there are no lines anymore,
exception <tt>End_of_file</tt> is raised and we send <tt>None</tt> on that same channel. After that, it's just the matter of waiting for one message on <tt>count_source</tt>, closing the file and printing the result:

```ocaml
let reader count_source lines_dest =
  let file = open_in "input.txt" in
  let rec aux () =
    let line = try  Some (input_line file)
               with End_of_file -> None    in
      sync (send lines_dest line);
      match line with
	| Some _ -> aux ()
	| None   -> let printed = sync (receive count_source) in
	    Printf.printf "The task wrote %i strings\n" printed;
	    close_in file
  in aux ()
```


The printer is also a simple loop. It keeps receiving messages on <tt>lines_source</tt>. If a message has structure <tt>Some v</tt>, then <tt>v</tt> is a line, print it and increment the counter. Otherwise, the message has structure <tt>None</tt>, which means that we're done, just send the number of lines on <tt>count_dest</tt>:

```ocaml
let printer lines_source count_target =
  let rec aux i =
    match sync (receive lines_source) with
      | Some line -> print_endline line; aux ( i + 1 )
      | None      -> sync (send count_target i)
  in aux 0
```


Finally, our main program creates both communication channels and backgrounds treatment of <tt>printer</tt>:

```ocaml
let _ =
  let count = new_channel ()
  and lines = new_channel ()
  in
  let _ = Thread.create (printer lines) count
  in reader count lines
```


Note that, had we decided to background treatment of <tt>reader</tt> instead, an additional synchronization would have been necessary to prevent the program from leaving once the main thread is over.


## Oforth


Oforth implements concurrency with tasks and communication between tasks using channels. Here, we create 2 channels (one for print, one to retrieve the number of lines).

When the file has been read and sent to channel chPrint, this channel is closed. All tasks waiting for an object in this channel are released and receive null. Then printing task returns number of printed lines into chCount channel for the main task.


```Oforth
import: parallel

: printing(chPrint, chCount)
  0 while( chPrint receive dup notNull ) [ println 1+ ] drop
  chCount send drop ;

: concurrentPrint(aFileName)
| chPrint chCount line |
   Channel new ->chPrint
   Channel new ->chCount

   #[ printing(chPrint, chCount) ] &

   aFileName File new forEach: line [ chPrint send(line) drop ]
   chPrint close
   chCount receive "Number of lines printed : " print println ;
```



## ooRexx


```ooRexx

queue = .workqueue~new
input = .stream~new("jabberwocky.txt")
output = .output

reader = .filereader~new(input, queue)
writer = .filewriter~new(output, queue)

::class workQueue
::method init
  expose queue stopped actionpending
  queue = .queue~new
  stopped = .false
  actionPending = .false

-- add an item to the work queue.  This is a
-- guarded method, which means this is a synchronized access
::method addItem guarded
  expose queue actionPending
  use arg item
  -- add the item to the queue
  queue~queue(item)
  -- indicate there's something new.  This is a condition variable
  -- that any will wake up any thread that's waiting on access.  They'll
  -- be able to get access once we exit
  actionPending = .true

-- another method for coordinating access with the other thread.  This indicates
-- it is time to shut down
::method stop guarded
  expose actionPending stopped
  -- indicate this has been stopped and also flip the condition variable to
  -- wake up any waiters
  stopped = .true
  actionPending = .true

-- read the next item off of the queue.  .nil indicates we've reached
-- the last item on the queue.  This is also a guarded method, but we'll use
-- the GUARD ON instruction to wait for work if the queue is currently empty
::method nextItem
  expose queue stopped actionPending
  -- we might need to loop a little to get an item
  do forever
    -- if there's something on the queue, pull the front item and return
    if \queue~isEmpty then return queue~pull
    -- if the other thread says it is done sending is stuff, time to shut down
    if stopped then return .nil
    -- nothing on the queue, not stopped yet, so release the guard and wait until
    -- there's something pending to work on.
    guard on when actionPending
  end

-- one half of the synchronization effort.  This will read lines and
-- add them to the work queue.  The thread will terminate once we hit end-of-file
::class filereader
::method init
  -- accept a generic stream...the data source need not be a file
  use arg stream, queue

  reply   -- now multithreaded

  signal on notready
  loop forever
     queue~addItem(stream~linein)
  end
  -- we come here on an EOF condition.  Indicate we're done and terminate
  -- the thread
  notready:
  queue~stop

-- the other end of this.  This class will read lines from a work queue
-- and write it to a stream
::class filewriter
::method init
  -- accept a generic stream...the data source need not be a file
  use arg stream, queue

  reply   -- now multithreaded

  loop forever
     item = queue~nextItem
     -- .nil means last item received
     if item == .nil then return
     -- write to the stream
     stream~lineout(item)
  end

```



## Oz


```oz
declare
  %% Helper function to read a file lazily.
  %% Returns a lazy list of lines.
  fun {ReadLines FN}
     F = {New class $ from Open.file Open.text end init(name:FN)}
     fun lazy {ReadNext}
        case {F getS($)} of
           false then nil
        [] Line then
           Line|{ReadNext}
        end
     end
  in
     %% close file when handle becomes unreachable
     {Finalize.register F proc {$ F} {F close} end}
     {ReadNext}
  end

  Count %% Will receive the number of lines
  PrinterPort
in
  %% Printer thread
  thread
     Stream
     Counter = {NewCell 0} %% mutable variable
  in
     PrinterPort = {NewPort ?Stream}
     for Line in Stream do
        case Line of eof then
           Count = @Counter
        else
           {System.showInfo Line}
           Counter := @Counter + 1
        end
     end
  end

  %% Send all lines to printer thread; make sure that eof is sent.
  try
     for Line in {ReadLines "input.txt"} do
        {Send PrinterPort Line}
     end
  finally
     {Send PrinterPort eof}
  end

  %% Sync on Count and print its value.
  {Wait Count}
  {Show Count}
```



## Perl


```perl
use threads;
use Thread::Queue qw();

my $q1 = Thread::Queue->new;
my $q2 = Thread::Queue->new;

my $reader = threads->create(sub {
     my $q1 = shift;
     my $q2 = shift;

     open my $fh, '<', 'input.txt';
     $q1->enqueue($_) while <$fh>;
     close $fh;
     $q1->enqueue(undef);

     print $q2->dequeue;
}, $q1, $q2);

my $printer = threads->create(sub {
     my $q1 = shift;
     my $q2 = shift;

     my $count;
     while (my $line = $q1->dequeue) {
         print $line;
         $count++;
     };

     $q2->enqueue($count);
}, $q1, $q2);

$reader->join;
$printer->join;
```



## Perl 6

{{works with|rakudo|2013-02-27}}

```perl6
sub MAIN ($infile) {
    $infile.IO.lines ==> printer() ==> my $count;
    say "printed $count lines";
}

sub printer(*@lines) {
    my $lines;
    for @lines {
	.say;
	++$lines;
    }
    $lines;
}
```

Concurrent units are established by use of the feed operator, which works much like an in-process object pipe.  Since the final feed goes to a variable declaration that belongs to the outer thread, it serves as a backchannel from the printer thread.  In this case the outer thread signals the desire for a line count by terminating the pipe to the printing thread.
(Note: soon these will be implemented with real threads in Perl 6, but this is currently emulated with coroutine semantics, aka lazy lists.)


## Phix

Busy wait version (using threads).
This program is included in the distribution as demo\rosetta\Synchronous_concurrency.exw, which also contains single step-lock and queue-less versions of this code.

```Phix
atom frThread,  -- file reader thread
     lcThread   -- line counter thread

sequence queue = {}
integer qlock = init_cs()

integer linecount = 1

procedure readfile()
object line
integer fn = open("input.txt","r")
    while 1 do
        line = gets(fn)
        enter_cs(qlock)
        queue = append(queue,line)
        line = atom(line)   -- kill refcount!
        leave_cs(qlock)
        if line then exit end if
    end while
    close(fn)
    wait_thread(lcThread)
    printf(1,"Lines read: %d\n",linecount)
    exit_thread(0)
end procedure

procedure countlines()
object line
    linecount = 0
    while 1 do
        enter_cs(qlock)
        if length(queue)=0 then
            leave_cs(qlock)
--          sleep(0.1)
        else
            line = queue[1]
            queue = queue[2..$]
            leave_cs(qlock)
            if atom(line) then exit end if
--          ?line
            linecount += 1
        end if
    end while
    exit_thread(0)
end procedure

frThread = create_thread(routine_id("readfile"),{})
lcThread = create_thread(routine_id("countlines"),{})

wait_thread(frThread)
puts(1,"done")
{} = wait_key()
```



## PicoLisp

PicoLisp has no threads, but synchronous background tasks and asynchronous
signal handlers, or coroutines.


### Using background tasks and signals

The following two tasks communicate via UDP, so in fact they don't need to run
within the same process and not even the same machine. "input.txt" would rather
be a device (like a named pipe or socket) than a plain file.

```PicoLisp
# Reading task (synchronous)
(task (open "input.txt")
   (let Fd @
      (if (in Fd (line T))             # More lines?
         (udp "localhost" 4444 @)      # Yes: Send next line
         (task (port T 4445)           # Else install handler
            (prinl (udp @) " lines")   # to receive and print count
            (task (close @)) )
         (udp "localhost" 4444 T)      # Send 'T' for "Done"
         (task (close Fd)) ) ) )       # Stop the task

# Printing task (asynchronous)
(sigio (setq "Sock" (port T 4444))
   (job '((Cnt . 0))
      (let? X (udp "Sock")
         (if (=T X)                    # Done?
            (prog
               (udp "localhost" 4445 Cnt) # Yes: Send count
               (sigio (close "Sock")) )   # and stop the task
            (println X)                # Else print line to stdout
            (inc 'Cnt) ) ) ) )         # and increment count
```

If the two cases of 'sigio' in the printing task are replaced with 'task',
that task would also be synchronous. The resulting behavior is the same.


### Using coroutines

Coroutines are available only in the 64-bit version.

```PicoLisp
(co 'unit1
   (yield)                       # Allow 'unit2' to start
   (in "input.txt"               # Read the file
      (while (line T)            # Send each line
         (yield @ 'unit2) ) )    # to 'unit2'
   (prinl
      (yield NIL 'unit2)         # Send 'NIL' for "Done", receive count
      " lines" ) )

(co 'unit2
   (let Cnt 0                    # Init counter
      (while (yield NIL 'unit1)  # Receive line
         (println @)             # Print it
         (inc 'Cnt) )            # Increment count
      (yield Cnt 'unit1) ) )     # Send count to 'unit1'
```




## Pony

Pony has concurrency baked into the language in the form of the actor model:

```Pony
use "files"

actor Main
  let _env: Env // The environment contains stdout, so we save it here

  new create(env: Env) =>
    _env = env
    let printer: Printer tag = Printer(env)
    try
      let path = FilePath(env.root as AmbientAuth, "input.txt")? // this may fail, hence the ?
      let file = File.open(path)
      for line in FileLines(file) do
        printer(line) // sugar for "printer.apply(line)"
      end
    end
    printer.done(this)

  be finish(count: USize) =>
    _env.out.print("Printed: " + count.string() + " lines")


actor Printer
  let _env: Env
  var _count: USize = 0
  new create(env: Env) => _env = env

  be apply(line: String) =>
    _count = _count + 1
    _env.out.print(line)

  be done(main: Main tag) => main.finish(_count)
```


Actors are scheduled asynchronously, but messages (implemented via the behaviours) are guaranteed to arrive in causal ordering.



## PureBasic

PureBasic uses Semaphores and Mutex's to coordinate threads.

```PureBasic
Enumeration
  #Write
  #Done
EndEnumeration

Structure commblock
  txtline.s
  Order.i
EndStructure

Global MessageSent=CreateSemaphore()
Global LineWritten=CreateSemaphore()
Global LinesWritten, com.commblock

Procedure Writer(arg)
  Repeat
    WaitSemaphore(MessageSent)
    If com\Order=#Write
      PrintN(com\txtline)
      LinesWritten+1
    EndIf
    SignalSemaphore(LineWritten)
  Until com\Order=#Done
EndProcedure

Procedure Reader(arg)
  Protected File=ReadFile(#PB_Any,OpenFileRequester("","input.txt","",0))
  While file And Not Eof(file)
    com\txtline=ReadString(File)
    com\Order=#Write
    SignalSemaphore(MessageSent)
    WaitSemaphore(LineWritten)
  Wend
  com\Order=#Done
  SignalSemaphore(MessageSent)
  WaitSemaphore(LineWritten)
  PrintN(Str(LinesWritten)+" lines written.")
EndProcedure

If OpenConsole()
  Define Thread1=CreateThread(@Reader(),0)
  Define Thread2=CreateThread(@Writer(),0)
  WaitThread(Thread1) And WaitThread(Thread2)
  Print("Press Enter to exit"):Input()
EndIf
```



## Python

Notes: instead of hardcoding the input and output files in the units, each unit is created with a file and read or write the given file.


```python
import sys
from Queue import Queue
from threading import Thread

lines = Queue(1)
count = Queue(1)

def read(file):
    try:
        for line in file:
            lines.put(line)
    finally:
        lines.put(None)
    print count.get()

def write(file):
    n = 0
    while 1:
        line = lines.get()
        if line is None:
            break
        file.write(line)
        n += 1
    count.put(n)

reader = Thread(target=read, args=(open('input.txt'),))
writer = Thread(target=write, args=(sys.stdout,))
reader.start()
writer.start()
reader.join()
writer.join()
```



### Using generators

{{trans|Ruby}}

```python
count = 0

def reader():
    for line in open('input.txt'):
        yield line.rstrip()
    print('Printed %d lines.' % count)

r = reader()
# printer
for line in r:
    print(line)
    count += 1
```


Note that the above accesses a variable from both paths of execution. To be more in the spirit of the task, we can actually communicate the count from the printer to the reader:
{{works with|Python|2.5+}}

```python
def reader():
    for line in open('input.txt'):
        yield line.rstrip()
    count = yield None
    print('Printed %d lines.' % count)

r = reader()

# printer
for count, line in enumerate(r):
    if line is None:
        break
    print(line)
try:
    r.send(count)
except StopIteration:
    pass
```



## Racket

Using thread mailboxes for communication between threads:

```racket

(define (reader)
  (for ([line (in-lines (open-input-file "input.txt"))])
    (thread-send printer-thread line))
  (thread-send printer-thread eof)
  (printf "Number of lines: ~a\n" (thread-receive)))

(define (printer)
  (thread-send reader-thread
               (for/sum ([line (in-producer thread-receive eof)])
                 (displayln line)
                 1)))

(define printer-thread (thread printer))
(define reader-thread  (thread reader))

(for-each thread-wait
          (list printer-thread reader-thread))


```



Using synchronous channels for communication between threads:

```racket

(define (reader out-ch result-ch)
  (for ([line (in-lines (open-input-file "input.txt"))])
    (channel-put out-ch line))
  (channel-put out-ch eof)
  (printf "Number of lines: ~a\n" (channel-get result-ch)))

(define (printer in-ch result-ch)
  (channel-put result-ch
               (for/sum ([line (in-producer channel-get eof in-ch)])
                 (displayln line)
                 1)))

(define lines-ch (make-channel))
(define result-ch (make-channel))
(define printer-thread (thread (lambda () (printer lines-ch result-ch))))
(define reader-thread  (thread (lambda () (reader lines-ch result-ch))))

(for-each thread-wait
          (list printer-thread reader-thread))

```



## Raven


```raven
'input.txt' as src_file

class Queue

    new list  as items
    condition as ready

    define item_put
        items push ready notify

    define item_get
        items empty if ready wait
        items shift

Queue as lines
Queue as count

thread reader
    "file://r:%(src_file)s" open each lines.item_put
    NULL lines.item_put count.item_get "reader: %d\n" print

thread writer
    0 repeat lines.item_get dup while
        "writer: %s" print 1+
    drop count.item_put

reader as r
writer as w
```



## Ruby

The task is to refactor this program to use two concurrent units.


```ruby
count = 0
IO.foreach("input.txt") { |line| print line; count += 1 }
puts "Printed #{count} lines."
```


The above program has no concurrency, because the printer is a block <tt>{ |line| print line; count += 1 }</tt> that can print only one line. (The reader calls block more than one time.) After the refactoring, the printer will print all lines, and the program will jump between the reader and the printer. For the refactoring, Fibers might be best, Continuations work if our Ruby is too old for Fibers, and Threads are another alternative.


----
'''Fibers.''' Ruby 1.9 gives Fiber to us. ''Fibers provide concurrency.'' Each fiber has its own stack (to hold nested function calls). A pair of fibers has a boss-vassal relationship: the boss fiber calls <tt>vassal.resume</tt> to jump to the vassal, and the vassal fiber calls <tt>Fiber.yield</tt> to jump to the boss.

{{works with|Ruby|1.9}}

```ruby
count = 0
reader = Fiber.new do
  IO.foreach("input.txt") { |line| Fiber.yield line }
  puts "Printed #{count} lines."
  nil
end

# printer
while line = reader.resume
  print line
  count += 1
end
```


* We create a fiber as the reader, and we use the current fiber as the printer.
* Our reader is a [[generator]], because <tt>reader.resume</tt> generates a <tt>line</tt>, and <tt>Fiber.yield</tt> passes the generated value.
* This generator allows <tt>Fiber.yield</tt> inside nested function calls (because a fiber has its own stack). We nested three calls: we have <tt>Fiber.yield</tt> inside our line block, inside <tt>IO.foreach</tt>, inside our fiber block.
* Because the reader and printer are in the same process, they can share the <tt>count</tt> variable. An alternate way is to use <tt>reader.resume(count)</tt> to pass the count, so <tt>Fiber.yield</tt> would return the count.
* If <tt>IO.foreach</tt> raises an IO error, then the reader dies, and <tt>reader.resume</tt> raises the same error in the printer. This is what we want. If we run this program with no <tt>input.txt</tt> file, then we see the error.


----
'''Continuations.''' Ruby 1.8 gives Continuation to us. (Ruby 1.9 still gives Continuation if we <tt>require 'continuation'</tt> from the standard library.) The trick is that you can continue a function call after you leave it. ''Continuations can provide concurrency.'' The problem is that continuations make spaghetti code with very confusing control flow.

{{libheader|continuation}}

```ruby
require 'continuation' unless defined? Continuation

count = 0
reader = proc do |cont|
  IO.foreach("input.txt") { |line| cont = callcc { |cc| cont[cc, line] }}
  puts "Printed #{count} lines."
  cont[nil]
end

# printer
while array = callcc { |cc| reader[cc] }
  reader, line = array
  print line
  count += 1
end
```


* The above program uses continuations almost like fibers. The reader continues the printer, and the printer continues the reader.
* The first call to <tt>reader[c]</tt> uses <tt>Proc#[]</tt> to start the reader; but later calls to <tt>reader[c]</tt> use <tt>Continuation#[]</tt> to continue the reader.
* The reader and printer share the same stack. The control flow when <tt>IO.foreach</tt> raises an IO error is very strange. The reader dies, and ''the original call to Proc#[]'' raises the same error in the printer.


----
'''Threads.''' Both Ruby 1.8 and Ruby 1.9 give Thread to us. ''Threads provide preemptive concurrency.'' The scheduler preempts threads and switches between threads, seemingly at random. Threads seem worse than continuations, because threads have unpredictable control flow, but we can use a Queue to restore some order. We use Thread with Queue.

{{libheader|thread}}

```ruby
require 'thread'

counts = Queue.new
lines = Queue.new
reader = Thread.new do
  begin
    File.foreach("input.txt") { |line| lines << line }
    lines << :EOF
    puts "Printed #{counts.pop} lines."
  ensure
    lines << nil
  end
end

# writer
count = 0
while line = lines.pop
  case line
  when String
    print line
    count += 1
  when :EOF
    counts << count
  end
end
reader.join
```


* We create a thread as the reader, and use the current thread as the writer.
* If a thread tries to pop an empty queue, then the thread waits until some other thread queues something.
* The queue of <tt>lines</tt> can become long; the worst case allows the reader to read the entire file before the printer pops the first line! If you wanted to prevent a long queue, a <tt>SizedQueue.new(5)</tt> would hold only 5 elements.
* If <tt>IO.reader</tt> raises an IO error, then the reader dies. The writer would deadlock on an empty queue after the reader dies. To prevent this deadlock, the reader ensures to queue a final <tt>nil</tt> before it dies. The writer uses this <tt>nil</tt> to break its loop and call <tt>reader.join</tt>. If the reader dies with an IO error, then <tt>reader.join</tt> raises the same error.


## Rust


{{works with|rustc 1.4.0-nightly|f84d53ca0 2015-09-06}}


```rust
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

use std::thread::spawn;
use std::sync::mpsc::{SyncSender, Receiver, sync_channel};

fn main() {
    let (tx, rx): (SyncSender<String>, Receiver<String>) = sync_channel::<String>(0);

    // Reader thread.
    spawn(move || {
        let file = File::open("input.txt").unwrap();
        let reader = BufReader::new(file);

        for line in reader.lines() {
            match line {
                Ok(msg) => tx.send(msg).unwrap(),
                Err(e) => println!("{}", e)
            }
        }

        drop(tx);
    });

    // Writer thread.
    spawn(move || {
        let mut loop_count: u16 = 0;

        loop {
            let recvd = rx.recv();

            match recvd {
                Ok(msg) => {
                    println!("{}", msg);
                    loop_count += 1;
                },
                Err(_) => break // rx.recv() will only err when tx is closed.
            }
        }

        println!("Line count: {}", loop_count);
    }).join().unwrap();
}
```



## Scala

[[Category:Scala examples needing attention]]
A possible implementation using Actors

```scala
case class HowMany(asker: Actor)

val printer = actor {
  var count = 0
  while (true) {
    receive {
      case line: String =>
        print(line); count = count + 1
      case HowMany(asker: Actor) => asker ! count; exit()
    }
  }
}

def reader(printer: Actor) {
  scala.io.Source.fromFile("c:\\input.txt").getLines foreach { printer ! _ }
  printer ! HowMany(
    actor {
      receive {
        case count: Int => println("line count = " + count)
      }
    })
}

reader(printer)
```



## Swift

Using Grand Central Dispatch and NSNotificationCenter


'''Reader.swift'''

```Swift
//
//  Reader.swift
//

import Foundation

class Reader: NSObject {
    let inputPath = "~/Desktop/input.txt".stringByExpandingTildeInPath
    var gotNumberOfLines = false

    override init() {
        super.init()
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "linesPrinted:",
            name: "LinesPrinted", object: nil)
    }

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    // Selector for the number of lines printed
    func linesPrinted(not:NSNotification) {
        println(not.object!)
        self.gotNumberOfLines = true
        exit(0)
    }

    func readFile() {
        var err:NSError?
        let fileString = NSString(contentsOfFile: self.inputPath,
            encoding: NSUTF8StringEncoding, error: &err)

        if let lines = fileString?.componentsSeparatedByString("\n") {
            for line in lines {
                NSNotificationCenter.defaultCenter().postNotificationName("Line", object: line)
            }
            NSNotificationCenter.defaultCenter().postNotificationName("LineNumberRequest", object: nil)

            while !self.gotNumberOfLines {
                sleep(1 as UInt32)
            }
        }
    }
}
```

'''Printer.swift'''

```Swift
//
//  Printer.swift
//

import Foundation

class Printer: NSObject {
    var numberOfLines = 0
    var gotRequestLineNumber = false

    override init() {
        super.init()
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "gotLine:",
            name: "Line", object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "lineNumberRequest:",
            name: "LineNumberRequest", object: nil)
    }

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    func gotLine(not:NSNotification) {
        println(not.object!)
        self.numberOfLines++
    }

    func lineNumberRequest(not:NSNotification) {
        self.gotRequestLineNumber = true
        NSNotificationCenter.defaultCenter().postNotificationName("LinesPrinted", object: self.numberOfLines)
    }

    func waitForLines() {
        while !self.gotRequestLineNumber {
            sleep(1 as UInt32)
        }
    }
}
```

'''main.swift'''

```Swift
//
//  main.swift
//

import Foundation

dispatch_async(dispatch_get_global_queue(0, 0)) {
    let printer = Printer()
    printer.waitForLines()
}

dispatch_async(dispatch_get_global_queue(0, 0)) {
    let reader = Reader()
    reader.readFile()
}

CFRunLoopRun()

```



## SystemVerilog



```SystemVerilog
program main;

 mailbox#(bit) p2c_cmd = new;
 mailbox#(string) p2c_data = new;
 mailbox#(int) c2p_data = new;

 initial begin
   int fh = $fopen("input.txt", "r");
   string line;
   int count;
   while ($fgets(line, fh)) begin
     p2c_cmd.put(0);
     p2c_data.put(line);
   end
   p2c_cmd.put(1);
   c2p_data.get(count);
   $display( "COUNT: %0d", count );
 end

 initial begin
   bit done;
   int count;
   while (!done) begin
     p2c_cmd.get(done);
     if (done) begin
       c2p_data.put(count);
     end
     else begin
       string line;
       p2c_data.get(line);
       $display( "LINE: %s", line);
       count++;
     end
   end
 end

endprogram
```



## Tcl

Uses the Thread package.

```tcl
package require Thread

# Define the input thread
set input [thread::create {
    proc readFile {filename receiver} {
	set f [open $filename]
	while {[gets $f line] >= 0} {
	    thread::send $receiver [list line $line]
	}
	close $f
	thread::send $receiver lineCount lines
	puts "got $lines lines"
    }
    thread::wait
}]
# Define the output thread
set output [thread::create {
    set lines 0
    proc line {string} {
	puts $string
	incr ::lines
    }
    proc lineCount {} {return $::lines}
    thread::wait
}]

# Connect everything together and start the processing
thread::send $input [list readFile "input.txt" $output]
```



## TXR


Using delimited-continuation-based <code>obtain</code> and <code>yield-from</code> to simulate co-routines, wrapped in some OOP.  A <code>thread</code> base class is derived into <code>consumer</code> and <code>producer</code>, both of which provide <code>run</code> methods. The <code>consumer</code> has a counter also, and <code>producer</code> holds a reference to a <code>consumer</code>.

When the objects are instantiated, their co-routines auto-start, thanks to the <code>:postinit</code> hook.

To get things going, we resume the producer via <code>pro.(resume)</code>, because we started that in a suspended state. This is actually not necessary; if we remove the <code>suspended t</code> from the <code>new</code> expression which instantiates the producer, we can remove this line. However, this means that the body of the <code>let</code> doesn't receive control. The producer finishes producing and then the <code>pro</code> variable is bound, and the final <code>(put-line ...)</code> expression evaluates. Starting the producer suspended lets us insert some logic prior to dispatching the producer.   We implicitly start the consumer, though, because it immediately suspends to wait for an item, saving its context in a continuation and relinquishing control.


```txrlisp
(defstruct thread nil
  suspended
  cont
  (:method resume (self)
    [self.cont])
  (:method give (self item)
    [self.cont item])
  (:method get (self)
    (yield-from run nil))
  (:method start (self)
    (set self.cont (obtain self.(run)))
    (unless self.suspended
      self.(resume)))
  (:postinit (self)
    self.(start)))

(defstruct consumer thread
  (count 0)
  (:method run (self)
    (whilet ((item self.(get)))
      (prinl item)
      (inc self.count))))

(defstruct producer thread
  consumer
  (:method run (self)
    (whilet ((line (get-line)))
      self.consumer.(give line))))

(let* ((con (new consumer))
       (pro (new producer suspended t consumer con)))
  pro.(resume)
  (put-line `count = @{con.count}`))
```



## UnixPipes

{{clarify task}}
the main process is the one started by cat file.
The subshell created by >(...) is the secondary process
the main pipeline waits for the secondary process to finish,
collects and prints the count. This falls a-foul of the requirement
that fork should not be used as every thing in pipes is done using
forks.


```bash
rm -f node ; mkfifo node
cat file | tee >(wc -l > node ) | cat - node
```



## Visual Basic .NET

This can be improved by adding a blocking Dequeue instead of spinning on TryDequeue.


```vbnet
Imports System.Threading

Module Module1

   Sub Main()
       Dim t1 As New Thread(AddressOf Reader)
       Dim t2 As New Thread(AddressOf Writer)
       t1.Start()
       t2.Start()
       t1.Join()
       t2.Join()
   End Sub

   Sub Reader()
       For Each line In IO.File.ReadAllLines("input.txt")
           m_WriterQueue.Enqueue(line)
       Next
       m_WriterQueue.Enqueue(Nothing)

       Dim result As Integer
       Do Until m_ReaderQueue.TryDequeue(result)
           Thread.Sleep(10)
       Loop

       Console.WriteLine(result)

   End Sub

   Sub Writer()
       Dim count = 0
       Dim line As String = Nothing
       Do
           Do Until m_WriterQueue.TryDequeue(line)
               Thread.Sleep(10)
           Loop
           If line IsNot Nothing Then
               Console.WriteLine(line)
               count += 1
           Else
               m_ReaderQueue.Enqueue(count)
               Exit Do
           End If
       Loop
   End Sub

   Private m_WriterQueue As New SafeQueue(Of String)
   Private m_ReaderQueue As New SafeQueue(Of Integer)

End Module

Class SafeQueue(Of T)
   Private m_list As New Queue(Of T)
   Public Function TryDequeue(ByRef result As T) As Boolean
       SyncLock m_list
           If m_list.Count = 0 Then Return False
           result = m_list.Dequeue
           Return True
       End SyncLock
   End Function
   Public Sub Enqueue(ByVal value As T)
       SyncLock m_list
           m_list.Enqueue(value)
       End SyncLock
   End Sub
End Class
```



## zkl


```zkl
fcn reader(fileName,out){
   n:=0; foreach line in (File(fileName)) { out.write(line); n+=1; }
   out.close();	// signal done
   Atomic.waitFor(out.Property("isOpen")); // wait for other thread to reopen Pipe
   out.write(n);
}
fcn writer(in){
   Utils.zipWith(fcn(n,line){ "%3d: %s".fmt(n,line).print() },[1..],in);
   in.open();  // signal other thread to send num lines read
   println("Other thread read ",in.read()," lines");
}

p:=Thread.Pipe();  // NOT Unix pipes, thread safe channel between threads
reader.launch("input.txt",p);
writer.future(p).noop();  // noop forces eval, ie sleep until writer finished
```

Light on error handling, easily fixed.
{{out}}
input.txt is just a code file

```txt

  1: to := Thread.Pipe(); from := Thread.Pipe();
  2: a:=fcn(to,from){
 ...
 15: }.launch(to,from);
 16:
 17: a.noop();
Other thread read 17 lines

```


{{omit from|AWK}}
{{omit from|GUISS|This can only be done, if there are apps installed that do this}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|Retro}}
{{omit from|ZX Spectrum Basic|Does not support concurrency.}}
