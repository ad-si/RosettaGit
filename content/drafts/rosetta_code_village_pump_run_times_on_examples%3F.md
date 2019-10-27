+++
title = "Rosetta Code:Village Pump/Run times on examples?"
description = ""
date = 2011-09-06T15:35:16Z
aliases = []
[extra]
id = 10459
[taxonomies]
categories = []
tags = []
+++

{{vptopic
|topic=Run times on examples?
|summary=Discussion on rules regarding adding run times to examples
}}
This page has initial content copied (with slight embellishments) from a [[User_talk:Paddy3118#Comparisons|user talk page]]. Please continue the discussion...
----
RC sets out to be a comparison site for whatever tools users apply the effort of comparing. One of the earliest pages I created tried to document typical invocation of a C compiler. I can't find it, now, though. --[[User:Short Circuit|Michael Mol]] 11:45, 6 September 2011 (UTC)

:Hi Michael, does this apply to my comment on preferably ''not'' adding detailed timings to solo program runs? If so, then maybe we need a [[Rosetta Code:Village Pump/Run times on examples?]] page to evolve some consensus? I, as you probably know, am a bit wary as you need to specify a huge amount of probably obscure information to get repeatable timings from different people and would prefer a maximum of:
:* Not mentioning stand-alone program timings that are less than two minutes and making other comments vague such as approx 5, 10 20 minutes or aprox 0.5/1/2/5 hours (or "it was done after a cup of tea/lunch/in the morning/...)
:* When there are two or more solutions in the one language then mentioning just a rounded comparison i.e.  X is approx. 1/2/5/10/100/1000/... times as fast as Y.
:* Discouraging timing comparisons between different languages.
:* Allowing detailed timings, if someone wants to, in the talk pages.
:* But generally - like now, leaving them out.

:We don't have the infrastructure of [http://shootout.alioth.debian.org/ The Computer Language Benchmarks Game], which is one of the best sites for comparing language run times, and even they tell users ''[http://shootout.alioth.debian.org/dont-jump-to-conclusions.php don't jump to conclusions]''.

:: Yeah, I jumped the gun by responding without seeing the context of your original discussion (I misunderstood it to refer to, e.g. different compilers' run-time support libraries). I agree 100%. --[[User:Short Circuit|Michael Mol]] 12:45, 6 September 2011 (UTC)


----
I'll note that I'd be happy to provide the infrastructure for such testing, if RC had the resources to do it. It currently doesn't, but that hasn't stopped such needs from being met in the past. Off the top of my head, the needed resources would be:
# A stable operating environment to perform the tests in. (Hey, is there grad student out there who'd like to try benchmarking in a deterministic environment?)
# Someone to write the tests. (Relatively simple, I think; use existing code on RC)
# Running the tests. Consisting of either
## Someone to automate running the tests. (Lots of time), or
## Someone to run the tests. (Lots and lots of time)
--[[User:Short Circuit|Michael Mol]] 15:35, 6 September 2011 (UTC)
