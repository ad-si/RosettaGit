+++
title = "Talk:Fractran"
description = ""
date = 2016-10-24T00:09:37Z
aliases = []
[extra]
id = 17118
[taxonomies]
categories = []
tags = []
+++

Should we create a category for this programming language?  Might be interesting.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 21:33, 22 January 2014 (UTC)
: Theoretically yes. Practically, probably not; writing Hello World in it would be prohibitively difficult (how to do meaningful I/O?) so the set of tasks it could do would be a bit too small. We can make the category when someone starts doing the tasks, not before. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 21:54, 22 January 2014 (UTC)

== "natural format"? ==

Why does the task have to tack on a text parsing requirement? It's irrelevant to the fraction Turing machine, and simply distracting. Why not let people focus on the important stuff? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:19, 23 January 2014 (UTC)
:I can argue this one both ways.  On the one hand, we generally don't try to hide the ugly design choices of language designers, but let people post solutions to demonstrate all those ugly design choices, because it makes our own languages look prettier. <tt>:-)</tt>  On the other hand, if Fractran <i>is</i> a programming language, one would expect it to have some parsing requirements. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 18:04, 23 January 2014 (UTC)

:It seems to be a simple thing to do. I took it to mean parsing a format similar to that used in the task description. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:46, 23 January 2014 (UTC)
::Saying you can't use % instead of / feels kinda like persecution to me. (After all, there's a slash in the middle of %, and fractions are ratios too.)  So in the spirit of liberality I've taken the liberty to remove the 'incomplete' from Haskell.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:18, 24 January 2014 (UTC)

:::Noted. But I will use this discussion page to voice my objection to this flagrant favouritism shown to the Haskel community in the ''least'' strident way available to me: humour! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:26, 24 January 2014 (UTC)


==Several formulae rendered invisible to many browsers by cosmetic edits==

Cosmetic edits made to the task page at 22:55, 6 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left several of the formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:48, 20 September 2016 (UTC)

: Visibility of task formulae restored on 20 October 2016 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 01:10, 20 October 2016 (UTC)

== Some fractions are equal to others... ==

In the statement ... ''if N.f is integral'' for a given fraction ''f'' there is a possible ambiguity. Suppose ''f = p/q'' then one can make the test by checking only if ''q'' divides ''N'' rather than performing the full calculation. But, this is equivalent only if ''p'' does not have ''q'' as a factor. Thus, 6/2 would be a hit only if two divides ''N'', whereas 3/1 will always be a hit as will ''N.f''. In other words, will the fractions always have their greatest common divisor divided out? Since input data are always suspect, should one enforce this so that the MOD(N,Q) = 0 approach will succeed? [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 00:09, 24 October 2016 (UTC)
