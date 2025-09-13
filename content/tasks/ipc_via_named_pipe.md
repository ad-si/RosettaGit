+++
title = "IPC via named pipe"
description = ""
date = 2015-04-14T19:50:53Z
aliases = []
[extra]
id = 10589
[taxonomies]
categories = ["task"]
tags = []
+++

[[wp:Named pipe|Named pipe]], or FIFO, is a way of providing inter-process communications (IPC). The task is to demonstrate how it works, create two pipes, say, "in" and "out" (choose suitable names for your system), and write a program that works the two pipes such that:
# Data written to the "in" FIFO will be discarded except the byte count, which will be added to a total tally kept by the program;
# Whenever another process reads the "out" FIFO, it should receive the total count so far.

Possible issues:
* Chances are you don't already have "in" and "out" pipes lying around.  Create them within your program or without, at your discretion.  You may assume they are already created for you.
* Your program may assume it's the sole reader on "in" and the sole writer on "out".
* Read/write operations on pipes are generally [[wp:Blocking (computing)|blocking]].  Make your program responsive to both pipes, so that it won't block trying to read the "in" pipe while leaving another process hanging on the other end of "out" pipe indefinitely -- or vice versa.  You probably need to either poll the pipes or use multi-threading.
* You may assume other processes using the pipes behave; specificially, your program may assume the process at the other end of a pipe will not unexpectedly break away before you finish reading or writing.


## C

```c
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>

/* Flag for systems where a blocking open on a pipe will block
   entire process instead of just current thread.  Ideally this
   kind of flags should be automatically probed, but not before
   we are sure about how each OS behaves.  It can be set to 1
   even if not needed to, but that would force polling, which I'd
   rather not do.
     Linux:	won't block all threads (0)
     OpenBSD:	will block all (1)
   Other OSes: ?
*/

#define WILL_BLOCK_EVERYTHING	0

#if WILL_BLOCK_EVERYTHING
#include <poll.h>
#endif

size_t tally = 0;

void* write_loop(void *a)
{
	int fd;
	char buf[32];
	while (1) {
#if WILL_BLOCK_EVERYTHING
		/* try to open non-block. sleep and retry if no reader */
		fd = open("out", O_WRONLY|O_NONBLOCK);
		if (fd < 0) { /* assume it's ENXIO, "no reader" */
			usleep(200000);
			continue;
		}
#else
		/* block open, until a reader comes along */
		fd = open("out", O_WRONLY);
#endif
		write(fd, buf, snprintf(buf, 32, "%d\n", tally));
		close(fd);

		/* Give the reader a chance to go away. We yeild, OS signals
		   reader end of input, reader leaves. If a new reader comes
		   along while we sleep, it will block wait. */
		usleep(10000);
	}
}

void read_loop()
{
	int fd;
	size_t len;
	char buf[PIPE_BUF];
#if WILL_BLOCK_EVERYTHING
	struct pollfd pfd;
	pfd.events = POLLIN;
#endif
	while (1) {
#if WILL_BLOCK_EVERYTHING
		fd = pfd.fd = open("in", O_RDONLY|O_NONBLOCK);
		fcntl(fd, F_SETFL, 0);  /* disable O_NONBLOCK */
		poll(&pfd, 1, INFTIM);  /* poll to avoid reading EOF */
#else
		fd = open("in", O_RDONLY);
#endif
		while ((len = read(fd, buf, PIPE_BUF)) > 0) tally += len;
		close(fd);
	}
}

int main()
{
	pthread_t pid;

	/* haphazardly create the fifos.  It's ok if the fifos already exist,
	   but things won't work out if the files exist but are not fifos;
	   if we don't have write permission; if we are on NFS; etc.  Just
	   pretend it works. */
	mkfifo("in", 0666);
	mkfifo("out", 0666);

	/* because of blocking on open O_WRONLY, can't select */
	pthread_create(&pid, 0, write_loop, 0);
	read_loop();

	return 0;
}
```



## Go


```go
package main

import (
        "fmt"
        "io"
        "log"
        "os"
        "sync/atomic"
        "syscall"
)

const (
        inputFifo  = "/tmp/in.fifo"
        outputFifo = "/tmp/out.fifo"
        readsize   = 64 << 10
)

func openFifo(path string, oflag int) (f *os.File, err error) {
        err = syscall.Mkfifo(path, 0660)
        // We'll ignore "file exists" errors and assume the FIFO was pre-made
        if err != nil && !os.IsExist(err) {
                return
        }
        f, err = os.OpenFile(path, oflag, 0660)
        if err != nil {
                return
        }
        // In case we're using a pre-made file, check that it's actually a FIFO
        fi, err := f.Stat()
        if err != nil {
                f.Close()
                return nil, err
        }
        if fi.Mode()&os.ModeType != os.ModeNamedPipe {
                f.Close()
                return nil, os.ErrExist
        }
        return
}

func main() {
        var byteCount int64
        go func() {
                var delta int
                var err error
                buf := make([]byte, readsize)
                for {
                        input, err := openFifo(inputFifo, os.O_RDONLY)
                        if err != nil {
                                break
                        }
                        for err == nil {
                                delta, err = input.Read(buf)
                                atomic.AddInt64(&byteCount, int64(delta))
                        }
                        input.Close()
                        if err != io.EOF {
                                break
                        }
                }
                log.Fatal(err)
        }()

        for {
                output, err := openFifo(outputFifo, os.O_WRONLY)
                if err != nil {
                        log.Fatal(err)
                }
                cnt := atomic.LoadInt64(&byteCount)
                fmt.Fprintln(output, cnt)
                output.Close()
        }
}
```



## PicoLisp


```PicoLisp
(call 'mkfifo "in" "out")              # Create pipes

(zero *Cnt)                            # Initialize byte counter

(unless (fork)                         # Handle "out" pipe
   (loop
      (out "out"
         (sync)
         (tell)
         (prinl *Cnt) ) ) )

(unless (fork)                         # Handle "in" pipe
   (let P (open "in")
      (loop
         (in P                         # Open twice, to avoid broken pipes
            (while (rd 1)                 # (works on Linux, perhaps not POSIX)
               (tell 'inc ''*Cnt) ) ) ) ) )

(push '*Bye '(call 'rm "in" "out"))    # Remove pipes upon exit
(wait)                                 # (Terminate with Ctrl-C)
```

Test:

```txt
$ line <out
0
$ echo abc >in
$ line <out
4
$ echo äöü >in
$ line <out
11
```



## Racket


```Racket
#lang racket

(define-values (in out) (make-pipe))

;; Thread loops through list of strings to send
;; and closes port when finished
(define t1 (thread (lambda ()
                     (for ([i (list "a" "test" "sequence")])
                       (display i out)
                       (sleep 1))
                     (close-output-port out))))

;; Blocking call to read char, if not EOF then loop
(define t2 (thread (lambda ()
                     (define cnt 0)
                     (let loop ()
                       (when (not (eof-object? (read-char in)))
                         (set! cnt (add1 cnt))
                         (loop)))
                     (display (format "Bytes Rx: ~a\n" cnt))
                     (close-input-port in))))

(thread-wait t1)
(thread-wait t2)
```


## Ruby

{{improve|Ruby|
* Find a way to report errors from inside ''open_sesame'', such as Errno::ENOENT.
* Check that open file is a FIFO: <code>foopipe.stat.pipe?</code>
}}

With [[OpenBSD]], we observe that open(2) a named pipe blocks ''all threads'' in a process. (This must be bug in thread library.) So, we fork(2) other process to call open(2), and apply UNIXSocket to send IO object.

```ruby
require 'socket'

# Ruby has no direct access to mkfifo(2). We use a shell script.
system '/bin/sh', '-c', <<EOF or abort
test -p in || mkfifo in || exit
test -p out || mkfifo out || exit
EOF

# Forks a process to open _path_. Returns a _socket_ to receive the open
# IO object (by UNIXSocket#recv_io).
def open_sesame(path, mode)
  reader, writer = UNIXSocket.pair
  pid = fork do
    begin
      reader.close
      file = File.open(path, mode)
      writer.send_io file
    ensure
      exit!
    end
  end
  Process.detach pid
  writer.close
  return reader
end

insock = open_sesame("in", "rb")
outsock = open_sesame("out", "w")
inpipe, outpipe = nil
count = 0
readers = [insock, outsock]
writers = []
loop do
  selection = select(readers, writers)
  selection[0].each do |reader|
    case reader
    when insock
      inpipe = insock.recv_io
      puts "-- Opened 'in' pipe."
      insock.close
      readers.delete insock
      readers.push inpipe
    when outsock
      outpipe = outsock.recv_io
      puts "-- Opened 'out' pipe."
      outsock.close
      readers.delete outsock
      writers.push outpipe
    when inpipe
      count += (inpipe.read_nonblock(4096).size rescue 0)
    end
  end
  selection[1].each do |writer|
    case writer
    when outpipe
      outpipe.puts count
      puts "-- Counted #{count} bytes."
      exit
    end
  end
end
```


Example run:

{| class="wikitable"
| style="vertical-align: top; width: 50%;" |
```txt
$ ruby count.rb
-- Opened 'in' pipe.
-- Opened 'out' pipe.
-- Counted 32 bytes.
$
```

| style="vertical-align: top; width: 50%;" |
```txt
$ echo 'This is line 1.' > in
$ echo 'This is line 2.' > in
$ cat out
32
$
```

|}


## Tcl


```tcl
# Make the pipes by calling a subprocess...
exec sh -c {test -p in || mkfifo in || exit 1;test -p out || exec mkfifo out}

# How many bytes have we seen so far?
set count 0

# Read side; uses standard fileevent mechanism (select() under the covers)
set in [open in {RDONLY NONBLOCK}]
fconfigure $in -translation binary
fileevent $in readable consume
proc consume {} {
    global count in
    # Reads only 4kB at a time
    set data [read $in 4096]
    incr count [string length $data]
}

# Writer side; relies on open() throwing ENXIO on non-blocking open of write side
proc reportEveryHalfSecond {} {
    global count
    catch {
	set out [open out {WRONLY NONBLOCK}]
	puts $out $count
	close $out
    }
    # Polling nastiness!
    after 500 reportEveryHalfSecond
}
reportEveryHalfSecond

# Run the event loop until done
vwait forever
```



## zkl

zkl doesn't open pipes but it knows about them (on Unix anyway as they are just a file). So, outside of the program, create two named pipes and read/write to them inside the program.
```zkl
pipe:=Thread.Pipe();  // NOT a Unix pipe, for inter-thread commication
fcn writeLoop(pipe){  // a thread
   out:=File("out","w");
   foreach tally in (pipe){ out.writeln(tally); out.flush(); }
   println("writeLoop done");
}.launch(pipe);

fcn readLoop(pipe){  // a thread
   tally:=0;
   in:=File("in","r").howza(3); // open for read, reading characters
   while(1){  // named pipe sets EoF after writer exits
      foreach c in (in){ pipe.write(tally+=1) } // read bytes until EoF
   }
   in.close();
   println("readLoop done");
}.launch(pipe);

while(1){ Atomic.sleep(10000); } // veg out while other talk
```

Terminal 1:

```txt

$ mkfifo in; mkfifo out
<wait until other two terminals get going>
$ echo "hello world" > in
$ cat ../Tests/lorem_ipsum.txt >in

```

In Terminal 2, start the program:

```txt
$ zkl bbb
```

Terminal 3:
There is pretty much no delay - character enters "in", "out" sees tally.

```txt

$ cat out
1
2
3
4
5
6
7
8
9
10
11
12
...
1391
1392
1393
1394
1395
1396
1397
1398
1399

```


