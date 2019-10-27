+++
title = "Talk:Runge-Kutta method"
description = ""
date = 2016-09-22T08:08:48Z
aliases = []
[extra]
id = 11524
[taxonomies]
categories = []
tags = []
+++

== Tableaus ==

I see that practical RK solvers are often defined in terms of a tableau, and I keep thinking it would be awesome to have this task work that way. (I've got some C++ code to do it, but it's ''no-way'' suitable for RC because it's both totally gigantic and by someone else.) –[[User:Dkf|Donal Fellows]] 14:35, 15 March 2012 (UTC)
:Adaptive step sizing, arbitrary order, and showing the solving of higher order / coupled equations would all be interesting, but I fear it would turn into more of a math problem than a programming one. I've done quite a bit of mechanical engineering related programming I could add on RC, but its just hard to say how much the average person would follow along with or care about (and many would just end up complicated exercises in matrices). Everyone should have a basic RK4 solver in their language though. That said, <strike>ill</strike> I'll follow suit in Ada if someone wants to up the complexity ante on this one. [[User:Xenoker|Xenoker]] 17:59, 15 March 2012 (UTC)

== [[Mathematica]] ==

Is the Mathematica solution correct? It appears to be the case that the engine used has produced an analytical solution of the DE, rather than using RK4. So far as I can tell, that's not playing by the rules of this task (even though it is otherwise the more useful option). The reason why this matters is because the point when people actually normally break out the RK methods is when getting an analytical solution is rather more problematic. –[[User:Dkf|Donal Fellows]] 23:17, 16 March 2012 (UTC)
:It appears to have directly gone for the analytical answer rather than RK4, which I would consider fair if it was an automated optimization, however it does not appear it was told to use RK4 or 100 segments in the first place. [[User:Xenoker|Xenoker]] 01:12, 17 March 2012 (UTC)

== Lambdas in the Python example ==

As it is, this is completely unreadable and unhelpful to someone who's trying to learn (which seems to be the goal of this wiki). [[User:Nihil|Nihil]] ([[User talk:Nihil|talk]]) 01:59, 8 September 2014 (UTC)

==All formulae rendered invisible to many browsers by white-space tidying on April 29 2016==

White space edits on the task page, including the injection of spaces around expressions in &lt;math&gt; tags, have rendered all formulae in the task description completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:31, 20 September 2016 (UTC)
:I have the same problem: works with Firefox, does not work with IE. A quick look at the contribs of this user shows it's not the first time. I reverted his edit on the RK task, we'll have to check the other ones. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 06:17, 22 September 2016 (UTC)

:: Sometimes there are useful textual edits mixed in with the under-tested cosmetic tweaks, so repair will, unfortunately, often need to be more partial and time-consuming than a full undo. 
:: (Another hope is that a later version of the MediaWiki processor might acquire an expanded range of accepted input patterns, and stop choking on things like the redundant spaces around expressions in &lt;math&gt; tags).
:: In the meanwhile, there is a list of the tasks that were still affected by this kind of issue as of Sep 21 2016, at:  http://rosettacode.org/wiki/User_talk:Rdm#Names_of_tasks_still_affected. 
:: One or two of those listed have since been manually edited to restore lost visibility of formulae. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:06, 22 September 2016 (UTC)
