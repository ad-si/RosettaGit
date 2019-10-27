+++
title = "Category:Programming paradigm/Event-driven"
description = ""
date = 2009-08-17T19:45:55Z
aliases = []
[extra]
id = 3415
[taxonomies]
categories = []
tags = []
+++

{{feature|Programming paradigm}}'''Event-driven programming''' is a programming paradigm or else a system architecture which decomposes the program into a set of event handlers. The primary source of the events is the hardware I/O. This includes timer events as well. The secondary source is the events signaled by the software.

The opposite to event-driven is polling architecture. Though both actually respond to an event in order to handle it, "event-driven" usually refers to the apparently asynchronous way the handler is called and to an encapsulation of the handler in the code, usually in the form of a subprogram.

The paradigm mixes [[declarative programming|declarative]] and [[imperative programming|imperative]] approaches on the event handler and emitter sides correspondingly.

Event-driven programming does not necessarily imply [[concurrent programming]]. In fact, very often events are handled synchronously on the context of a single [[task]]. Furthermore event-driven architectures were used in the past as a substitute for proper multitasking support, for example in [[Windows]] and MS-DOS prior to 95 and NT.

In a truly multitasking environment event-driven programming is exposed to serious problems, especially to deadlocks. To other problems belong:

* unpredictable, often poor, behavior with respect to the response time and stack memory use;
* complexity of the arguments and results passing, especially between multiple tasks;
* handlers composition. Though some event-driven systems provide tools for routing, filtering events, chaining handlers etc;
* low abstraction level. (This is usually alleviate when an even-driven architecture is wrapped into an [[object-oriented programming|OO model]]).

Event-driven programming is quite popular in [[GUI]] design. Many GUI widget libraries have event-driven architecture. It is also used in database programming in the form of [http://en.wikipedia.org/wiki/Database_trigger triggers]. In systems programming hardware communication is predominantly event-driven per hardware nature.
