+++
title = "Talk:Equilibrium index"
description = ""
date = 2016-11-09T20:11:54Z
aliases = []
[extra]
id = 9895
[taxonomies]
categories = []
tags = []
+++

Some of the "one pass" solutions, such as in Perl6 and Python examples, are dubious.  Both use hash tables to store temp sums ("hash" in Perl, "dict" in Python), so as to save one pass through the input array because the result requires a single dictionary lookup.  In reality, however, hash table insertion can be quite expensive, and because all indices are inserted into the hash somewhere, it's almost garanteed to be no smaller than just storing the sums in a flat list.  It is probably more efficient, both speed- and space-wise, to just run a second pass through the input list or a stored summation list to pick out the right indices -- it's n array lookup vs n dictionary lookup, generally much faster, without even considering low level things like cache consistency.
: I remember thionking just that when I added it to Python, but at the time I was trying to have Python versions of most of the techniques shown so added it anyway. If a 2Gig X86 machine was reading data off a paper tape then maybe it would be worth consideration :-) --[[User:Paddy3118|Paddy3118]] 20:41, 7 June 2011 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 20:07, 5 June 2016 ==

Under-tested cosmetic edits made to the task page at 20:07, 5 June 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left most of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:04, 22 September 2016 (UTC)

: Visibility of server-side formula graphics now restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:11, 9 November 2016 (UTC)
