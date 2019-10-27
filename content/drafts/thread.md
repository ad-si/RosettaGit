+++
title = "Thread"
description = ""
date = 2009-08-21T19:41:54Z
aliases = []
[extra]
id = 2552
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]A '''thread''' is a particular flow of control within a [[process]]. A thread shares the address space with other threads in the same process, but each thread gets its own call stack and set of registers. Switching between threads within a process is a much lighter-weight operation than switching between processes.

Conceptually, there are two major types of thread: ''cooperatively'' scheduled threads and ''preemptively'' scheduled threads. With cooperatively scheduled threads, it is up to the thread to manually yield control to the task scheduler (though this usually also happens on I/O) and with preemptively scheduled threads, every thread may be interrupted and suspended by the scheduler at any point. The advantage of cooperatively scheduled threads is that it is easy to make sure that control is only transferred to other threads when the state of the system is logically consistent with higher-level constraints, but preemptively scheduled threads can make much better use of additional hardware. Thread library implementations can by ''native'' (provided by the [[:Category:Operating Systems|operating system]]) or ''green'' (an abstraction provided solely by the language or threading library). [[Erlang]] and [[Forth]] provide their own threading mechanisms, and [[Java]] VMs can often be configured to use either native or green threading (later versions of Java have [[wp:Green_threads#Green_threads_in_Java_virtual_machine|dropped green threading]]). Native thread systems are also usually preemptively scheduled, and green thread implementations can be either cooperative or preemptive. It is also possible to simulate cooperative threading with coroutines.

Programs can be "[[Simple concurrent actions|multi-threaded]]", where certain parts of the programs will compete for system resources. A multi-threaded program can run multiple operations concurrently. With single-core processors, multi-threading does not usually provide much of a performance advantage (non-performance advantages such as simpler expression of some algorithms are wholly distinct), but with multiple cores, a computer can run the threads concurrently and do multiple programs' work at once. Typically, this requires a native thread implementation for such hardware to be taken advantage of.

Threads are useful for servers, where each client connecting to a server will "spawn" a new thread for its own operations.

One of the best known threading abstractions is '''[[POSIX]] threads''' (pthreads), which are widely used on [[UNIX]]-like systems.

==See also==
* [[wp:Thread_%28computer_science%29|Wikipedia]]
