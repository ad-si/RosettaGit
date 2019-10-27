+++
title = "Talk:Echo server"
description = ""
date = 2016-01-28T10:44:43Z
aliases = []
[extra]
id = 4156
[taxonomies]
categories = []
tags = []
+++

==Similarities==
For some reason, this task resembles half (only the server part) of another task, [[Distributed program]]; I believe that with few changes every server code given there could be "imported" here. Therefore basically this task seems an almost-duplicate. So it seems to me. --[[User:ShinTakezou|ShinTakezou]] 17:48, 13 May 2009 (UTC)

I think they are different -- this task emphasizes byte-stream network programming and multitasking, that task emphasizes high-level communication and message passing. I had planned to create a task something like this myself, and I also created Distributed Program. --[[User:Kevin Reid|Kevin Reid]] 19:14, 13 May 2009 (UTC)

I wrote this task to emphasize network server basics. It's meant to be something that someone wanting a simple net protocol implementation could take, adapt and use. I ''really'' hope that it's one that many languages can do a good implementation of. —[[User:Dkf|Dkf]] 20:11, 21 May 2009 (UTC)

== Ruby example ==

That Ruby example is wrong; the while loop accepts a new connection each time round and assumes that each time round the loop it will either close the new, connected socket or read a single line from it (and echo it) before dropping it unceremoniously without closing it. I find it hard to accept that this is correct! In particular, if you try to connect and send two lines of data to it, only the first one will be echoed. –[[User:Dkf|Donal Fellows]] 13:43, 1 December 2009 (UTC)
: It was fixed by [[User:Schultzi]]. Thanks! –[[User:Dkf|Donal Fellows]] 21:54, 6 December 2009 (UTC)

== AutoHotkey Client ==

I've moved the client code to a sub-page. It was quite long, and it was not really part of the task. –[[User:Dkf|Donal Fellows]] 08:31, 9 January 2010 (UTC)

== Java example ==
I tested the Java example on Ubuntu; correctly handled 1000 parallel clients and empty lines, but a `cat /dev/random | nc localhost 12321` broke it. Should it be considered OK or not? --[[User:Silverweed|Silverweed]] ([[User talk:Silverweed|talk]]) 14:38, 3 September 2014 (UTC)

== C++ example ==

It would be nice to see a C++ example on here. Yes, I know of the POSIX sockets. However, seeing as the Boost library is more or less
the "unofficial" standard library and Boost.Asio takes care of the platform dependent work (ie. WinSock, POSIX) I believe it to be beneficial to have an example of that, or obtain permission to use the C++11 example from Boost's docs, which uses lambda functions. --[[User:CMM87|CMM87]] ([[User talk:CMM87|talk]]) 10:44, 28 January 2016 (UTC)
