+++
title = "Talk:Input/Output for Lines of Text"
description = ""
date = 2016-08-13T21:53:22Z
aliases = []
[extra]
id = 17067
[taxonomies]
categories = []
tags = []
+++

== Perl 6 ==

Do we have to be explicit about $*IN and $*OUT?   What about just:


```perl6
say get() xx get;
```


?--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 06:51, 9 January 2014 (UTC)
:But it was saying that you have to call an intermediate method, so I took it very literally  as "method" rather than function.  I also took it very literally that it didn't specify that you call a user-defined method.  So I called a helper method already predefined on the handle. <tt>:-)</tt>  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 05:36, 10 January 2014 (UTC)

==Task needs clarification==

What is "''passing it to a method as an intermediate step''" supposed to mean?

(See also the discussion section above this one, which demonstrates how that sentence can be ambiguous/confusing.) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 21:53, 13 August 2016 (UTC)
