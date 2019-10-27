+++
title = "Talk:Chat server"
description = ""
date = 2016-08-06T13:42:19Z
aliases = []
[extra]
id = 9165
[taxonomies]
categories = []
tags = []
+++

==Handling slow connections==
It seems to me that there is the possibility that a slow/faulty connection could delay anyone sending messages. Suppose there are 4 people on the server, A, B, C and D. If B's connection is screwy, then any time A, C or D attempt to send a message, the server will either block on the broadcast to B, or the message will not be sent completely. Looking at the Python implementation, I see that connection is set to non-blocking, so the default behavior is the latter (incomplete message). I don't know about the other implementations, but I think this behavior should be clarified. Should one bad connection be able to disable the communications for the whole server, and is one slow connection receiving incomplete messages acceptable?

The proper way to handle this would be have a thread or threads to handle broadcasting to ensure that messages sent are complete and that they don't delay normal input processing, but maybe this is beyond the scope of the problem? --[[User:Paul.miner|Paul.miner]] 16:44, 15 January 2011 (UTC)
: The solution should provide a ''usable'' implementation. What this implies may be always fuzzy, but implementors could describe possible limitations and explain their reasons. --[[User:Abu|Abu]] 07:17, 16 January 2011 (UTC)
:: Sounds good --[[User:Paul.miner|Paul.miner]] 06:51, 17 January 2011 (UTC)
: With the Tcl solution, a slow write is not (much) of a problem; the output is automatically queued until such time as the client can accept it (at a cost of some memory, of course; that's the source of the “much”, and it's possible to monitor it if necessary though that's rare). I don't know whether the Python solution is the same; it's the sort of thing that really benefits from deep event loop integration in the IO handling layer. (For the record, the Tcl solution should be “usable”. It lacks some features, e.g. authentication, but they're unlikely to alter the core of the chat app much.) –[[User:Dkf|Donal Fellows]] 09:49, 17 January 2011 (UTC)
:: That's all right. Authentication is not required for this task (keep it simple :). --[[User:Abu|Abu]] 10:06, 17 January 2011 (UTC)

==How to test it==

I've never used telnet before, thought the <code>telnet</code> command ''is'' installed on my Linux box.

Can someone give me a run-down of how one might use that client-side command to connect and chat with some of these server implementations, so that I could use as a testing aid while developing my own server implementation for this task?
--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:11, 6 August 2016 (UTC)

: simply enter <code>telnet localhost xxxx</code>, where "xxxx" is the port used by the server. Some of the examples use fixed port numbers, others require you to add it via parameter. C example uses port 7070, so <code>telnet localhost 7070</code> should do the trick.

:: It works, thanks! --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 13:42, 6 August 2016 (UTC)
