+++
title = "Talk:IPC via named pipe"
description = ""
date = 2011-10-04T00:43:02Z
aliases = []
[extra]
id = 10595
[taxonomies]
categories = []
tags = []
+++

== Tricky to open "out" ==
This is tricky! The “<tt>in</tt>” reader side is normal asynchronous IO (the pipe becomes readable when there's data available) but the “<tt>out</tt>” writer side is very awkward because you ''can't'' open the writer side of a FIFO if there is no reader there. You've either got to use a blocking open() — which might or might not work with a threaded solution — or deal with the failure to open() it in non-blocking mode by polling regularly. Tricky stuff! –[[User:Dkf|Donal Fellows]] 09:38, 30 September 2011 (UTC)

: <s>Actually I don't think <code>O_WRONLY|O_NONBLOCK</code> on a fifo will ever succeed on a POSIX system.</s>(unless there is already a reader)  On Linux, you can <code>O_RDWR</code> open a fifo, and it will not block (because the program itself is the reader then), but then you can't <code>select</code> it for write readiness because it ''always'' returns immediately with success.  Luckily here one thread blocking on open will not hold up the entire process, so it's relatively easy to deal with. --[[User:Ledrug|Ledrug]] 11:44, 30 September 2011 (UTC)

:: You only want to have one end of each named pipe open in the process; using <code>O_RDWR</code> is opening both ends, and that's semantically wrong anyway (irrespective of whether or not it “works” with select()). It's irritating that O_NONBLOCK doesn't work for the pipe writer — POSIX [http://pubs.opengroup.org/onlinepubs/009604599/functions/open.html specifies that behavior] for reasons that are unclear to me, which scuppers the whole plan of using select() to wait for a reader, and the rationale doesn't go into enough depth — but at least it means that we can handle it with polling to give the right overall effect. (Sockets are nicer.) –[[User:Dkf|Donal Fellows]] 12:23, 3 October 2011 (UTC)

::: O_RDWR is semantically wrong, but POSIX specifically left it undefined instead of an error, which might be an intentional shortcut left to individual OS implementations.  O_NONBLOCK|O_WRONLY behavior, as I vaguely understand it, has something to do with the pipe buffer management, though I'm very unsure about it. --[[User:Ledrug|Ledrug]] 00:43, 4 October 2011 (UTC)

== OpenBSD blocks the entire process ==

```ruby
$ irb -rthread
irb(main):001:0> q = Queue.new; Thread.new {q << open("out", "w")}
=> #<Thread:0x0000020da6a630 run>
irb(main):002:0> ^C
irb(main):002:0> q.empty?
=> true
```


With [[OpenBSD]], I observed that <code>open("out", O_WRONLY)</code> does block the entire process. This is a bug in the thread library, because an IO system call ''must'' block only the current thread; it is ''wrong'' to also block other threads. This <code>irb</code> session comes from Ruby [[MRI]] 1.9.4dev above OpenBSD. My first line tries to open "out" in another thread. This blocks the other thread, but because of this bug, also blocks my entire <code>irb</code> process! I must hit Control-C to cancel the open.

OpenBSD is also among the last systems to implement threads in userspace. OpenBSD [http://www.openbsd.org/cgi-bin/man.cgi?query=pthreads&apropos=0&sektion=3&manpath=OpenBSD+4.9&arch=i386&format=html pthreads(3)] is a "user-level library" that [http://archives.zmanda.com/amanda-archives/viewtopic.php?t=4815 uses non-blocking IO with the kernel]. We know that <code>open("out", O_WRONLY|O_NONBLOCK)</code> is not possible, so I guess that the library blocks the entire process. This bug should only happen with OpenBSD. Other OS (like Linux, FreeBSD, NetBSD) might have kernel threads.

Some interpreters, like Ruby [[MRI]] 1.8.x, have "green threads". These interpreters might also use non-blocking IO, so they might block the entire process when opening "out" with any OS. --[[User:Kernigh|Kernigh]] 04:21, 2 October 2011 (UTC)
: Eh so on OpenBSD opening a fifo for readonly also blocks whole process? If so, what's the behavoir of <code>read()</code> or <code>select()</code>? --[[User:Ledrug|Ledrug]] 23:23, 2 October 2011 (UTC)
:: Yes, <code>open("in", O_RDONLY)</code> also blocks whole process. Other functions like read(), usleep(), poll() and select() seem to block only current thread. --[[User:Kernigh|Kernigh]] 01:05, 3 October 2011 (UTC)
::: Good then.  If <code>read()</code> would have also blocked entire process, reading on "in" pipe could have been a serious problem in case of a slow writer because of pipe buffer.  As it stands, I guess it's not a problem. --[[User:Ledrug|Ledrug]] 01:19, 3 October 2011 (UTC)
