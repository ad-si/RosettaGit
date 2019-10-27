+++
title = "Talk:Count the coins"
description = ""
date = 2015-09-04T08:30:13Z
aliases = []
[extra]
id = 10739
[taxonomies]
categories = []
tags = []
+++

== Python vs D ==

The previous claim of Python/psycho being faster than D was a little curious.  Could it be because the D code uses bigint throughout, while python will only switch to bigint when the values exceed 64 bits?  If so, that's another reason to not make speed comparisons. --[[User:Ledrug|Ledrug]] 07:22, 30 October 2011 (UTC)
:why not? the only reason not to make such comparisons it to avoid getting into a debate about it.
:otherwise if one version is faster than another it is useful to know. even better would be though to also understand why. can the D version be rewritten easily to not use bignum for smaller values? if not then that is an important point to understand when comparing languages. people claim that C is faster, (i don't know about D) but they forget to explain that in order to get the faster speed you also have to know how to take advantage of that. so if the comparison shows that python is faster for naive code then i think that is something useful.--[[User:EMBee|eMBee]] 09:25, 30 October 2011 (UTC)
::that said, the python speed comparison involves psyco which is not a standard part of using python, the same comment could be made about many examples. therefore i am not sure if such comments are adding any value unless the task involves solving a problem efficiently.--[[User:EMBee|eMBee]] 09:38, 30 October 2011 (UTC)
: > ''why not? the only reason not to make such comparisons it to avoid getting into a debate about it.''
: Well, now you are in a debate about it.
: One definitely can write D code where bignum is only used when necessary, though obviously it won't be as convenient as in languages which can manage that by itself as in Python.  But you don't see people making claims like "coding in language A is about 2.7 times easier than in language B", only the speed comparisons, which is misleading by not telling the whole story.  Also, unless you are confident both examples are well written within the bounds of reasonable effort, such comment doesn't really tell much. --[[User:Ledrug|Ledrug]] 14:24, 30 October 2011 (UTC)
:: yes, i agree, and i think the comparison should be removed because of that. though it might be useful to mention that the D version could be made faster by not using bignum for smaller values. that should cover all the details necessary for anyone to make their own picture when they compare solutions.--[[User:EMBee|eMBee]] 03:18, 31 October 2011 (UTC)

== Python vs C ==

The second Python version gives:
99341140660285639188927260001

For:
make_change(1000000, [1,2,5,10,20,50,100,200])

While the C version gives:
8.033207543e+24

That means:
8033207542650321391101281

: Turns out I forgot to add the high 8 bytes for the 128-bit addition.  Surprising how it didn't bork until the very last one. --[[User:Ledrug|Ledrug]] 02:31, 31 October 2011 (UTC)

== rare vs. uncommon coins ==

Not a programming comment ... (just adding my two-bits, er, I mean 8% of that), but I would say that halves are not very rare, just uncommon.   Around these parts (upper mid-west of the US), both dollar coins and halves are common.   Now, what's very rare is a twenty-¢ piece, half disme, or a three-¢ piece.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:17, 24 March 2012 (UTC)

What's ever rarer than hen's teeth is a <big>½</big>-cent piece --- despite that there were over 7.8 million of those coins minted.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:56, 2 September 2015 (UTC)



-----


Also, the REXX versions support fractional cents.   Support was added to allow specification of a half-cent and quarter-cent as   '''1/2'''   and   '''1/4'''. 

All fractional cents can be entered as (for instance)   '''.5'''   '''.25'''     (with or without superfluous leading zeroes).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:36, 1 September 2015 (UTC)
