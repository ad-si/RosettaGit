+++
title = "Talk:Rendezvous"
description = ""
date = 2016-08-22T17:29:36Z
aliases = []
[extra]
id = 4151
[taxonomies]
categories = []
tags = []
+++

This task is problematic to attempt because its description is currently tied very closely to how Ada conceives of it to be done. —[[User:Dkf|Dkf]] 18:57, 12 May 2009 (UTC)
: From the birth of it I am wondering if using dbus would be possible to achieve the same purpose and being task compliant... and why not named pipe...? or simply a socket...? Is a synchronous "echo server" (like [[Echo Server]], but synchronous as said) usable in the same way a ''rendezvous'' can be used? (Just throwing questions as usual:D) --[[User:ShinTakezou|ShinTakezou]] 13:32, 14 May 2009 (UTC)
:: If I remember right (and it's a few years since I studied this) with the “Rendezvous” communication style:
::* The sender sends a message and waits for the receiver to reply; will only commence execution once the receiver responds.
::* The receiver can choose when to accept the message and how to respond to it.
::* Sender and receiver are synchronized (with sender idle) for the duration of the processing of the message. It is from this synchronization that the name “rendezvous” originates.
::* It has a lot in common with the communication model in [[wp:Calculus of communicating systems|CCS]], except (of course) for the fact that values are passed as arguments and returned as a result.
::* I do not remember if Ada allows accepting of messages dependent on what the values of some arguments are, but other (admittedly much more obscure) languages do allow such things within the general communications model; it's a language feature, not a communications feature.
::* I believe the sender can't formally constrain how the receiver responds.
:: Does this (rather sketchy) summary help? —[[User:Dkf|Dkf]] 08:07, 19 May 2009 (UTC)

:* Sender: SendMessage(port, msg); Wait(port); ... (get the answer, do something ecc.)
:* Receiver: loop... for some reason, at some point: GetMessage(port, msg); if (msg != NULL) { do something with the message, prepare an answer, SendMessage(port, answer); } ...

:This can be done, and should keep the task synchronized through the communication "port"; but of course there nothing like "arguments" passed or returned... there's a "message" flying from A to B and viceversa. <cite>it's a language feature, ...</cite> means it can't be implemented (as the task requires it) in e.g. [[C]]?
: Yes helped thanks, but I am always full of doubts... still without any idea about which languages I know can do that "as language feature"... currently I believe none of "mine". --[[User:ShinTakezou|ShinTakezou]] 15:50, 20 May 2009 (UTC)
::You certainly can implement it [[C++]] using a corresponding [[OS]] library for threading and interlocking. (Clearly C++ is not a concurrent language, so any tasking should rely on [[OS]] services). It is possible to do, because I myself implemented rendezvous support for C++. Unfortunately I cannot contribute the code, because it is a part of a licensed library. The main obstacle for C++ is not synchronization, but a lack generic copy for classes. So it is impossible to re-raise an exact copy of an exception on another context. You start with a definition of an abstract C++ class, Task (or Thread if you prefer that name). Then you define methods Request_Rendezvous, Accept_Rendezvous etc. You will need a queue of rendezvous requests, usually doubly linked list. Limiting the waiting time for rendezvous to be serviced is a bit tricky, but nothing impossible.
::The discussion above about messages looks a bit strange, because rendezvous aren't messages. surely, it is possible to implement rendezvous on top of some messaging framework, but that would be rather stupid, because the main advantage of the rendezvous is its light weight compared to messages. The key features of a rendezvous is synchronism and absence of marshaling. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:34, 21 May 2009 (UTC)
:::You're coupled very closely to the Ada concept of rendezvous there, and yes, I've actually written a language (no, it can't participate in RC; it's a hardware language and really obscure; executing the code outside a simulator requires a silicon foundry...) that uses them so I do know what they are. At the base level, a rendezvous is this:
:::*Sender sends a message and blocks, waiting for a reply.
:::*Receiver can choose which of many pending messages to receive – it will receive exactly one of them – and is able to execute (possibly substantial amounts of) code in order to create a response; a response ''must'' eventually be generated (but “eventually” could be infinitely far off).
:::That's it. It's possible to use expression guards on receives, but hardly necessary (and it greatly complicates the hardware implementation ;-)). None of this says anything about exceptions; at this level of modeling, an exception is just a kind of response. (Which suggests that the task itself shouldn't require exceptions; they're part of how you implement error responses, not a necessary part of the rendezvous itself.)
:::Anyway, I'm revisiting this because I just wanted to note (possibly for myself in the future, possibly for someone else) that I think [[Go]] could do a very nice implementation of this task. It's <code>[http://golang.org/doc/go_spec.html#Select_statements select]</code> statement (combined with channels and buffers of course) looks an eminently suitable tool. –[[User:Dkf|Donal Fellows]] 09:59, 16 November 2009 (UTC)

==See also==
Is the second link, to some facebook page, relevant?  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 02:01, 22 August 2016 (UTC)
:Good catch. Not relevant. About the band Rendezvous. That was added anonymously in Nov. 2009 and has been there ever since. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 17:29, 22 August 2016 (UTC)
