+++
title = "Talk:Distributed programming"
description = ""
date = 2011-06-19T15:02:10Z
aliases = []
[extra]
id = 4828
[taxonomies]
categories = []
tags = []
+++

I used IRC for the AutoHotkey entry. Is that allowed? [[User:BR|BR]]
:Whether it's allowed or not I think it would be better if it were on its own page ([[Distributed program/AutoHotkey]]) and that page simply had a link on this page. --[[User:Mwn3d|Mwn3d]] 15:50, 9 September 2009 (UTC)
::Done. --[[User:Glennj|glennj]] 16:37, 9 September 2009 (UTC)
:: Part of the problem is that he included the source of a [http://www.autohotkey.com/forum/topic35575.html library] the program depends on.  Talked to him on IRC, suggested he use Template:libheader. --[[User:Short Circuit|Michael Mol]] 16:55, 9 September 2009 (UTC)
:::I moved WinSock2.ahk to [[WinSock2.ahk]], is it short enough now to put back in the main page? [[User:BR|BR]]
::::7000 bytes is still pretty big. I wonder if maybe the Python examples could be broken out too. We don't need more problems with loading large pages. It'd be best to get the especially long examples as they come up rather than try to fix the problem after the fact. --[[User:Mwn3d|Mwn3d]] 17:06, 9 September 2009 (UTC)

== Insufficiently general? ==

A lot of the examples are slapped with this label.  Now what is a "sufficient" one?  One that can pass arbitrary sized data blocks?  One that can also mark native/network endianess?  One that can annotate the data's structure?  Or even with extended attributes/metadata?  Can we just send XML along with schema in utf-8 which must be fully strict, hence very likely to be correct but going to be horribly slow?  Maybe this task needs some review. --[[User:Ledrug|Ledrug]] 03:55, 18 June 2011 (UTC)

Hi. I wrote the original task. My intent was that it should show the use of ''distributed programming'' as opposed to simply being able to use the network — we have other tasks for that. The facilities used to accomplish the task should be suitable for ''performing a complex task distributed across several machines''. Or something like that. Trying to be sufficiently specific about sufficiently general, I'd say it should be a protocol/library which at least supports a reasonable set of common data structures, and preferably has a notion of messages/RPCs, such that the author of the example does not have to ''invent a protocol'', especially not a data serialization scheme, as opposed to just spreading their program across the network. 

I've also expanded the task description a bit. I tried to avoid wording it exclusively.

As to your specific examples — I would say that anything where the application has to think about "data blocks" is far too low-level. Passing XML chunks around might qualify — ''if'' it goes with a library/facility for conveniently using XML as a data structure, ''and'' has some reasonable way to have communications between multiple independent components of the distributed program, i.e. remote object/subsystem identifiers.

I'm thinking that it might have been a good idea to define a specific application to be written distributedly, rather than leaving it as "just pass messages"; one which makes the task easiest to accomplish given the sort of facilities this task is intended to demonstrate. Maybe I should write a draft task "Distributed programming 2"?

—[[User:Kevin Reid|Kevin Reid]] 04:20, 18 June 2011 (UTC)
: I wasn't actually serious when saying "XML".  If your data flow between nodes is relatively small, maybe, with high traffic even gigabit netlinks often get congested on large clusters, converting binary data into tagged text is just going to make it so much worse, not to mention the effort in parsing it.  But enough with that, about the current task, I think a specific goal would help; something that would require two nodes pass messages back and forth several times, each time making different requests, for example.  A truly general protocol is too tall a bill: look at HTTP, after all these years some servers or clients still choke at different bits received.  A task v2 may be a good idea, too.  --[[User:Ledrug|Ledrug]] 02:12, 19 June 2011 (UTC)

:: I just removed the incorrect tag from the Python example as Sockets work with text and Python can serialize to text using one of its in-built modules with higher efficiency than using XML. --[[User:Paddy3118|Paddy3118]] 06:27, 19 June 2011 (UTC)

:::I just took a look at that example — I would consider it incorrect because it (a) doesn't ''actually use'' the pickle module, and (b) does not have any facility for independent components. The Pyro, Spread, and XML-RPC examples are perfectly fine; I would just remove the sockets example since the point of this task is to work at a higher level than for the [[Sockets]] task. —[[User:Kevin Reid|Kevin Reid]] 15:02, 19 June 2011 (UTC)
