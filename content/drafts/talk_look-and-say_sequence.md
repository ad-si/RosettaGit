+++
title = "Talk:Look-and-say sequence"
description = ""
date = 2011-10-13T03:47:37Z
aliases = []
[extra]
id = 4179
[taxonomies]
categories = []
tags = []
+++

==Start of Sequence==
In example output might be best shown as '1' rather than the '11' of some examples? --[[User:Paddy3118|Paddy3118]] 06:01, 17 May 2009 (UTC)
:You mean, every example should show the "generator" '1', instead of starting with '11'? --[[User:ShinTakezou|ShinTakezou]] 11:35, 17 May 2009 (UTC)
::Yes, the WP article starts from 1 and it just seems more natural to me. It wouldn't be a big fix to those language examples that give sample output. --[[User:Paddy3118|Paddy3118]] 11:42, 17 May 2009 (UTC)

----

==AutoHotkey / Untested Examples==
Should we allow untested examples? There is always the stupid error that may creep in which makes an example that hasn't even been run too prone to error. 

Unless the comment means "untested, but I am about to test it now", then maybe contributors should refrain from adding code that even they are so unsure of?

It would be courteous to run code for a task of this complexity, (although the task is not complex), and check it gives some of the results required. --[[User:Paddy3118|Paddy3118]] 06:13, 27 November 2009 (UTC)
:All these untested examples of mine are from computers that can't run the program. Problem is, I generally forget. I do add <nowiki>{{untested|language}}</nowiki>, though. I'm not "so unsure" of it, I think it'll work, but I haven't tried it. When I get home, I'll run though all the untested AHK examples, when I have time, I'll run through all the untested examples I can run.

::Thanks BR, it is appreciated. --[[User:Paddy3118|Paddy3118]] 05:23, 28 January 2010 (UTC)

==Run-length Encoding==
This task is actually identical to the [[Run-length_encoding]] task, applied iteratively to strings of digits. In fact, the easiest/best solution is simply to re-apply the RLE task here. I think this should be mentioned in the task description. As it is, some of the code here is far more complicated than it needs to be, because the identity of the two tasks is not pointed out.

I do think it's OK to keep this as a separate task, though, since the Conway sequence is definitely an interesting application of RLE in its own right. --[[User:Snoman|Snoman]] 20:20, 11 July 2010 (UTC)

: Since Paddy3118 added a note about RLE, I went ahead and expanded it a bit. Hope that's OK  --[[User:Snoman|Snoman]] 04:23, 12 July 2010 (UTC)

:: Thanks :-)
--[[User:Paddy3118|Paddy3118]] 04:59, 12 July 2010 (UTC)

== PicoLisp ==

For the Picolisp solution, what happens if input sequence has an element repeated 10 or more times? --[[User:Ledrug|Ledrug]] 03:47, 13 October 2011 (UTC)
