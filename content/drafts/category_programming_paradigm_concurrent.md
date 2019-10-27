+++
title = "Category:Programming paradigm/Concurrent"
description = ""
date = 2009-08-12T19:38:42Z
aliases = []
[extra]
id = 3386
[taxonomies]
categories = []
tags = []
+++

{{feature|Programming paradigm}}'''Concurrent programming''' is programming with multiple [[task]]s. The major issues of concurrent programming are:

* Sharing computational resources between the tasks;
* Interaction of the tasks.

Objects shared by multiple tasks have to be safe for concurrent access. Such objects are called '''protected'''. Tasks accessing such an object interact with each other indirectly through the object.

An access to the protected object can be:

* Lock-free, when the task accessing the object is not blocked for a considerable time;
* Blocking, otherwise.

Blocking objects can be used for task synchronization. To the examples of such objects belong:

* Events;
* [[Mutex]]es and [[semaphore]]s;
* Waitable timers;
* Queues.

Objects encapsulating tasks, and thus able to change their state asynchronously, are called '''[[Active object|active]]'''.

Tasks can also interact directly by calling each other. This interaction can be:

* Asynchronous (message passing, mailbox, interrupt, task abort);
* Synchronous (rendezvous, remote call).

A synchronous call from one task to another is called '''[[rendezvous]]'''. The caller is blocked until the callee becomes ready to accept the rendezvous. Upon a rendezvous the caller may pass the parameters to the callee and get the results back after the completion of the rendezvous. A remote call is similar to rendezvous but also includes marshaling parameters and the results from one task to another.

A protected object can be implemented with a task exclusively accessing the object while all other tasks enter rendezvous with that task in order to change the object state. This synchronization model is called '''monitor'''.

Concurrent programming is considerably more difficult than non-concurrent programming. Concurrent access to objects may result in a [[race condition]]. Task interaction and synchronization are exposed to the problems of live-locks, deadlocks, priority inversion, meeting deadlines.
