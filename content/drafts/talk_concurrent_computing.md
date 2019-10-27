+++
title = "Talk:Concurrent computing"
description = ""
date = 2010-11-26T14:22:45Z
aliases = []
[extra]
id = 1918
[taxonomies]
categories = []
tags = []
+++

==Random order?==
Is random order what is meant here, or is "any order" sufficient? --[[User:Short Circuit|Short Circuit]] 00:16, 6 February 2007 (EST)
:Random order means that the order of output is not determined at compile time. The order of output may differ from one execution of the program to another due to differences in timing between the concurrent units providing the output. --[[User:Waldorf|Waldorf]] 11:47, 7 February 2007 (MST)
::"Undetermined" might be the more appropriate word.  At least one of the programming examples attempts to enforce random behavior. --[[User:Short Circuit|Short Circuit]] 14:17, 7 February 2007 (EST)
:::Random is what I wanted. The Ada example I provided creates three separate tasks. Each one calculates a random number between 0.0 and 1.0. The task then delays the number of seconds corresponding to that random number. A delay of 0.5 lasts 0.5 seconds. There are two goals to "Simple concurrent actions". Those goals are to demonstrate the syntax for defining concurrent behavior within a single program, and the syntax for creating a random sleep or delay. --[[User:Waldorf|Waldorf]] 17:59, 7 February 2007 (MST)
::::I see.  You might want to explain that in the task; I thought the task's purpose was simply to demonstrate forking. --[[User:Short Circuit|Short Circuit]] 20:33, 7 February 2007 (EST)
:::::What do you need to be clarified? I specified threads, tasks or co-routines. It can be argued that forking is a form of concurrency, which is why I specified threads, etc. Please let me know how you would like the task description to be clearer. There is currently a separate task to demonstrate forking. I have plans to explore more aspects of concurrency with tasks demonstrating synchronous and asynchonous communication between threads, etc. --[[User:Waldorf|Waldorf]] 19:27, 7 February, 2007 (MST)
::::::OK.  I think I understand now.  Your task description is probably fine. --[[User:Short Circuit|Short Circuit]] 08:19, 8 February 2007 (EST)
:::::: AFAIK Co-routines are deterministic because there is only one thread of execution at any point of time. (deterministic output). [[User:Rahul|Rahul]] 14:48, 9 December 2008 (UTC)

== Race conditions? ==

It seems to me that none of the examples so far make sure that there's no attempt to output two strings at the same time. Do all the languages have reentrant output routines, or are there examples with subtle bugs in here? --[[User:Ce|Ce]] 09:33, 28 February 2008 (MST)
:I'm 99% sure Java will not output two strings at once. The [[JVM]] handles it. I don't know enough about the other languages to answer for them. --[[User:Mwn3d|Mwn3d]] 09:43, 28 February 2008 (MST)
::Interesting. I believe it is not an issue since ''writing'' to a stream should be ''atomically'' done, or done in a thread-safe way. The worst that can happen is messing up output, but race conditions or deadlocks or any other bad thing should not occur (at least, if messing up output is not a bad thing! EnRosejotCotayde?!) Consider that the same ''stream'' (stdout) can be ''transparently'' used by several processes (e.g. I am running Kate editor from the same shell I use to compile code, so sometimes stderr of Kate is intermixed with stdout and stderr of other tasks) --[[User:ShinTakezou|ShinTakezou]] 18:50, 17 December 2008 (UTC)
::Hm, maybe I am wrong, since for different processes the ''file'' descriptor is replicated; nonetheless they are always attached to the same terminal, and I am still thinking that race conditions are not possible. --[[User:ShinTakezou|ShinTakezou]] 19:01, 17 December 2008 (UTC)

== What are "concurrent threads"? ==

Conceptually speaking, unless you have multiple CPUs, there can be only one "thread" that is executing at any one time.  This suggests a possible approach for implementing this task in languages which do not "support threading":  One could implement a simplistic thread scheduler, and then use that to implement the task.

Consider also languages which support concurrent syntax without necessarily haven gotten around to implementing support for dispatching across multiple CPUs.

This leaves me wondering, what does this task specification really mean, in a language agnostic sense?  --[[User:Rdm|Rdm]] 15:38, 24 November 2010 (UTC)
: "in a language agnostic sense" ... I don't know. I'd muse that it mostly means, "of minimally-synchronized execution contexts", but then we'd have to come up with a meaning for the constraint 'minimal'. --[[User:Short Circuit|Michael Mol]] 18:40, 24 November 2010 (UTC)
: If the language/runtime supports executing over multiple CPUs, that's what should be done for this task. Otherwise, if the language/runtime conceptually supports concurrency but doesn't ''actually'' do so, it should be noted in the text for that solution that this is the case (probably along with a note as to what version of the lang/rtlib this is referring to, so that this is known to be an issue at that particular point in time instead of something that is an issue for all eternity). –[[User:Dkf|Donal Fellows]] 16:42, 25 November 2010 (UTC)
:: Ok, I have taken a stab at this... --[[User:Rdm|Rdm]] 14:22, 26 November 2010 (UTC)
