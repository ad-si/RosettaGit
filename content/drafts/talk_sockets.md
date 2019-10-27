+++
title = "Talk:Sockets"
description = ""
date = 2010-02-23T12:24:13Z
aliases = []
[extra]
id = 3236
[taxonomies]
categories = []
tags = []
+++


### UNIX Shell and UnixPipes

Aren't they the same?! I mean: it seems that UnixPipes are ''one-liner'' for a unix shell, which is the same interpreter that can interpret a "file" containing commands for the shell... Piping is a rather common way to do things on *n*x shells, from the command line or inside a script... --[[User:ShinTakezou|ShinTakezou]] 00:53, 9 December 2008 (UTC)
: There was one contributor who did all the "UnixPipes" entries several months ago. Personally, I'd be happy moving them all as alternate solutions under UNIX Shell. Maybe discuss on the Village Pump? --[[User:IanOsgood|IanOsgood]] 02:07, 9 December 2008 (UTC)
: Do take look at the UnixPipes talk page on discussion about this. (I was at that time studying the pipelines in CMS and the early Unix history of pipes.). Rather than Village pump, I would prefer the talk page of UnixPipes as the forum :) [[User:Rahul|Rahul]] 08:52, 9 December 2008 (UTC)

== C example: testing with nc ==

I changed the command for testing the C example from <code>netcat -l -p 256</code> to <code>nc -l 256</code>. From nc(1):
<lang>     nc [-46DdEhklnorStUuvz] [-e IPsec_policy] [-I length] [-i interval]
        [--no-tcpopt] [-O length] [-P proxy_username] [-p source_port]
        [-s source_ip_address] [-T ToS] [-w timeout] [-X proxy_protocol] [-x
        proxy_address[:port]] [hostname] [port]
```

<lang>     -p source_port
             Specifies the source port nc should use, subject to privilege
             restrictions and availability.  It is an error to use this option
             in conjunction with the -l option.
```

