+++
title = "Talk:Object serialization"
description = ""
date = 2013-10-22T07:57:00Z
aliases = []
[extra]
id = 4813
[taxonomies]
categories = []
tags = []
+++

Serializing object instances which have no state does not seem very meaningful.  You could trivially satisfy this task in some languages merely by naming the objects.

Meanwhile, when I think about this issue, a variety of possibilities occur to me, for state.  We could even introduce stateful classes and then [by implication] the classes would also need to be serialized -- but I do not think most of the examples do anything like this.

Can we update this task with a little bit of state?  Perhaps, we can add a method which reports the time the object was first created?  Or would that break all the existing implementations and thus be a bad idea?

[[User:Rdm|Rdm]] 20:25, 2 September 2009 (UTC)

:Try serializing an object graph; the inter-object links would be a reasonable piece of state. Or you could use the labels on the nodes as the state. I'd keep the construction short though; this is a serialization task, not a "build a graph" task after all. —[[User:Dkf|Donal Fellows]] 05:38, 3 September 2009 (UTC)

For non-object oriented languages, could we allow an object alternative? The serialization of something of equal complexity to an object, like a closure. I understand it is possible to create a new task, but I would like to avoid the linked list situation. [[User:bengt]] Mon Oct 21 21:37:29 CEST 2013

: If you still feel like doing this, note that even languages which are not themselves "object oriented" can implement something that could be called "objects" and "classes". For example, a C module can be treated as an object with relatively minimal effort (but serializing such a thing would require additional effort of some sort, and might also depend on platform-specific details).

:: Since Erlang has serialization built in, this could be a pretty example. If I have to add objects and classes (not built in) it would hide the interesting bits. The Algol example seems to use records, not objects. If my understanding is correct, than I could base Erlang on that. [[User:bengt]] Tue Oct 22 09:45:55 CEST 2013
