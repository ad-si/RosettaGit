+++
title = "Talk:Subleq"
description = ""
date = 2017-11-14T02:08:36Z
aliases = []
[extra]
id = 19061
[taxonomies]
categories = []
tags = []
+++

Just want to get some eyes on this and maybe a couple more implementations before releasing it..
[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 01:34, 27 April 2015 (UTC)

==Code points?==
You mention code points and characters as well as giving character numbers. Should you just state that for this example use ASCII? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:33, 28 April 2015 (UTC)
: My example uses ASCII; I'll add language to that effect. But there's no reason to limit the implementation to ASCII.  One that handled arbitrary Unicode code points would still run it fine.  An EBCDIC machine might have a problem with it.. [[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 00:55, 28 April 2015 (UTC)

:: I've added a REXX example that handles both (within the same program), and is machine (ASCII or EBCDIC) agnostic.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:08, 14 November 2017 (UTC)

==Promoting==
Haven't seen any other complaints about the task, and we've got several implementations added, so I've promoted it out of draft status. [[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 17:51, 29 April 2015 (UTC)

== Ada character input ==

I'm not an Ada programmer, but the [[Subleq#Ada|Ada sample]] looks wrong to me. In the ''read input'' step it uses <code>IIO.Get</code> to obtain the input, which I would have thought would read in an integer value, when it should just be reading a character. Is that a bug, or am I mistaken?
--[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 22:30, 22 September 2015 (UTC)
: You appear to be correct.  I've added an incomplete marker to the entry with a description of the problem.  Thanks.  --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 22:45, 22 September 2015 (UTC)
