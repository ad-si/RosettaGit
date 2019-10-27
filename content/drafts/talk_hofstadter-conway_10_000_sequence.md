+++
title = "Talk:Hofstadter-Conway $10,000 sequence"
description = ""
date = 2010-12-09T16:09:22Z
aliases = []
[extra]
id = 6246
[taxonomies]
categories = []
tags = []
+++

==Work in progress==
I still have a pretty picture and some links to add tonight, but I intend the task aims to stay the same. --[[User:Paddy3118|Paddy3118]] 09:22, 9 March 2010 (UTC)

:Yep, The New York Times seems to have got the ''exact'' details of the competition  wrong. I was wondering why my maxima were under 0.55 so quickly when the NYT mentions n>2**31. See [http://www.jstor.org/stable/2324028 here], which agrees with [http://mathworld.wolfram.com/Hofstadter-Conway10000-DollarSequence.html this]. Does the draft status of the task allow me to change its goals with impunity ;-)

:--[[User:Paddy3118|Paddy3118]] 05:48, 10 March 2010 (UTC)

==Mallows' Number==

According to [http://mathworld.wolfram.com/Hofstadter-Conway10000-DollarSequence.html Mathworld], the correct number is 1489, yet some of the entries are giving different results. Shouldn't they be marked as incorrect?
:Well, the only ones that are correct according to the task description are the ones giving 1490, but that's because the task description is incorrect.  The task description has n >= p whereas the Mathworld page you mention says i > n, not i >= n.  So I think we'll need to fix both the task and the programs that were written to that spec.  Oddly, at least one of the programs that gets 1489 (C#) appears to get the right answer for the wrong reason; it's returning a result using 0-based indexing, which compensates for adding one to match the bogus task description.  The Ada and Algol algorithms are truly incorrect, insofar as they are computing the last *maximum* that was larger than .55, not the last ratio.  a(1487) is larger than a(1489), but a(1489) is still above .55, so wins as the last one. --[[User:TimToady|TimToady]] 21:37, 8 December 2010 (UTC)
:I've gone and fixed all the ones that were just off-by-one from the bogus task description.  The C# one doesn't really need fixing, if the +1 is taken as compensating for 0-based arrays rather than calculating a different p.  The Ada and Algol entries are now correctly marked incorrect.  <tt>:)</tt> --[[User:TimToady|TimToady]] 22:26, 8 December 2010 (UTC)

::Thanks for the cleanup. I can only think that I copied a duff equation in one of my sources; which is odd, as I used more than one source when researching the task? (I could have just inserted an error but I prefer to blame some nebulous 'other'). --[[User:Paddy3118|Paddy3118]] 16:09, 9 December 2010 (UTC)
