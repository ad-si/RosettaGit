+++
title = "Talk:Hickerson series of almost integers"
description = ""
date = 2016-03-12T11:18:41Z
aliases = []
[extra]
id = 17028
[taxonomies]
categories = []
tags = []
+++

==OEIS==
I stuck the first five nearest integers from the series into The On-Line Encyclopedia of Integer Sequences whereupon I discovered it as series [http://oeis.org/A034172 A034172] and the search for terms mentions [http://oeis.org/search?q=1+3+13+75+541+4683+47293+545835+7087261+102247563&sort=&language=&go=Search more series]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:02, 1 January 2014 (UTC)

== range 1 to 17 ==

Why does the reference article claim 1 to 17 when it also has 'almost integers' only up to 15? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 11:59, 4 January 2014 (UTC)

:Why indeed? I just put it down to a hasty compilation of the page. It did point out to me that there was a task to be done though :-)
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:35, 4 January 2014 (UTC)

:I just made this point to the MathWorl team. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:30, 4 January 2014 (UTC)

::Very well. Why not The function is (erroneously) said to produce...--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:32, 5 January 2014 (UTC)

:::Because I am not sure if they have some weird/esoteric definition of almost integer. I usually come to them with my maths problems so allowed for them to be "right" in a way unfamiliar to me. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:00, 5 January 2014 (UTC)

==Perl 6 comment==
Hi Timtoady, I put the comment <code># Looks like you failed 2 tests of 17</code> outside the <nowiki>
```txt
...
```
</nowiki> block (but still in the output section) just to emphasise that it was not output printed by the program but something added in after. Or so I thought after scanning the Perl 6 source for that line as code. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:18, 4 January 2014 (UTC)
:That summary is output by the Test module we're using.  Test Anything Protocol (TAP) has a long and storied history in Perl—most Perl modules come with oodles of tests, so it's pretty baked into the culture to use Test (or one of its derivatives) whenever you can.  So there's no explicit print of the summary; it just happens automatically on any test exit with less than full success. It's a comment so that TAP will ignore it. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 03:58, 5 January 2014 (UTC)

::Thanks. I get it now. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:16, 5 January 2014 (UTC)

==Clojure?==
Perhaps the Clojure entry was deleted by mistake. Too many edits in a short time? -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 19:51, 4 January 2014 (UTC)
: Apparently so: I changed < pre > to < /pre > at the bottom :-( --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:00, 4 January 2014 (UTC)

==Haskell==

I refactored the Haskell example as it used a function that did too many things, so I split it up. The output now has less unecessary text, tell me if a more verbose output is required. --[[User:Zorro1024|Zorro1024]] ([[User talk:Zorro1024|talk]]) 11:18, 12 March 2016 (UTC)
