+++
title = "Talk:Closest-pair problem/C"
description = ""
date = 2011-06-20T03:35:41Z
aliases = []
[extra]
id = 8584
[taxonomies]
categories = []
tags = []
+++

your code does NOT RUN when i compile with devc or Turbo C???? (unsigned comment added by 113.22.126.190 at 21:06, 24 October 2010)
: Correct it? Try setting compiler flags for compatibility with a specific C standard? --[[User:Short Circuit|Michael Mol]] 13:11, 25 October 2010 (UTC)
:: "The code does not run"... is like pretending to derive solar physics by the statement "the sun shines"... By the way "devc" (Dev C++ IDE?) usually is used with (an old version of) gcc. On my test machine (GNU/Linux) it runs, except for some "evil dataset" that someone gave me once upon a time... I've inspected the code with valgrind, debugged it, ... but I was not able to unwind the flow that gives the problem... I know this code hides some oddity somewhere. Likely the better thing is to rewrite it from scratch, but I've not the courage yet! :) — [[User:ShinTakezou|ShinTakezou]] 17:52, 30 May 2011 (UTC)

== Propose replacing code ==

I suggest replace current code sample with rewritten code below.  Reasons:
1. It doesn't segfault with 200,000 points or more;
2. It's shorter and quite a bit faster;
3. It's cleaner IMO.
<snip: code removed from talk> --[[User:Ledrug|Ledrug]] 03:34, 18 June 2011 (UTC)

I am not in love with my own code, expecially when it does not work properly!:D Go replace it, if this one works (I am going to test it, but I trust it works)! (Note: there's no explicit "segfault with 200,000 points or more"... I'll keep my code so a day maybe I'll know why this happens:D) --[[User:ShinTakezou|ShinTakezou]] 14:54, 18 June 2011 (UTC)
