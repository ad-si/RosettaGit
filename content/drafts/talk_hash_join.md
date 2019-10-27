+++
title = "Talk:Hash join"
description = ""
date = 2014-03-07T20:08:16Z
aliases = []
[extra]
id = 16793
[taxonomies]
categories = []
tags = []
+++

I've downgraded to a draft task for now; we need more implementations (preferably independent ones!) so that we can be sure that a sufficient common understanding if the task exists. Once there are four language implementations, we can promote back. Also, what about language/runtime systems with built-in hash table support? –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 22:47, 30 November 2013 (UTC)
:Agree on the draft status for now.  And despite the, er, fireworks accompanying its creation, I suspect it'll be a good task.  I don't see how the built-in-ness of hashes plays one way or the other.  The task merely assumes that an appropriate hash implementation will be used, whether built-in or imported or implemented as part of the solution.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 04:05, 3 December 2013 (UTC)
:: And it is now a full task, with 5 implementations (right now). I'll do a bit more editing (adding in a print requirement and giving test data) but that won't break the existing implementations. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 10:48, 12 December 2013 (UTC)

==clarification needed==

Is Popeye's entry to be '''not''' listed, either because it has no nemesis, or because it wasn't in the 2nd relation list? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:41, 17 December 2013 (UTC)

: '''Popeye''' shouldn't be in the resulting relation because it isn't in the second relation. See also [[wp:Join_%28SQL%29#Inner_join]]. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 19:49, 7 March 2014 (UTC)

(If not, I'll enable the '''if''' statement that bypasses the check for the above condition.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:43, 17 December 2013 (UTC)

: The above REXX code ('''if''' statement) was removed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:08, 7 March 2014 (UTC)

Can the nemeses (plural) be listed on one line (as the REXX example shows)?   To me, it looks cleaner, more succint, less screen (output) clutter. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:41, 17 December 2013 (UTC)

== Is identity a hash? ==

If the easiest way of implementing this is to use the identity function as the hash function, would that qualify? Why or why not? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:29, 6 January 2014 (UTC)
