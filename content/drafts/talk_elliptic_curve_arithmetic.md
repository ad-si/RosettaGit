+++
title = "Talk:Elliptic curve arithmetic"
description = ""
date = 2016-09-21T19:14:07Z
aliases = []
[extra]
id = 20711
[taxonomies]
categories = []
tags = []
+++

== what is secp256k1 ? ==
What is   '''secp256k1'''   (as mentioned in the Rosetta Codes task's preamble: 

:::     ''You will use the a and b parameters of secp256k1, ... ''. 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:18, 4 April 2016 (UTC)

I think that's a reference to https://en.bitcoin.it/wiki/Secp256k1 --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:33, 5 April 2016 (UTC)

== multiple additions vs. multiplication ==
(Regarding the   ''extra credit''   part of the task.)


Has anybody done more research on performing   '''N'''   additions versus multiplication   (as the   '''EchoLisp'''   example has done?

There certainly seems to be a difference on how the   ''multiplication''   is implemented.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:25, 4 April 2016 (UTC)



-----



Initially, I performed   '''N'''   additions   (via the   '''add'''   function)   and kept getting much different results than the other programming examples   (for 12345 repetitions). 


I think it would be beneficial if the first ten multiples   (1 ──► 12)   would be shown so we could compare   ''true addition''   via   ''multiplicative''   results.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:32, 4 April 2016 (UTC)


==First formula now invisible on standard OS X browsers==
May need tidying up to achieve formula visibility on the OS X platform. Problems may include flanking of LateX expressions with redundant white space inside &lt;math&gt; tags [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:25, 16 September 2016 (UTC)

Specifically, the formula y^2 = x^3 + a x + b was hidden to most browsers by an under-tested cosmetic edit at 20:19, 7 June 2016 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:13, 21 September 2016 (UTC)
