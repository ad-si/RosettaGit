+++
title = "Talk:Cycle detection"
description = ""
date = 2016-03-03T16:16:13Z
aliases = []
[extra]
id = 20051
[taxonomies]
categories = []
tags = []
+++

== output ==
Wouldn't it be sufficient just to print the cycle? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 14:23, 26 February 2016 (UTC)

Not a bad idea. My choice of output was influenced by the needs of an algorithm that uses Cycle detection as a subroutine. 
Printing the cycle would make it easier to test and visualize the results.
--[[User:Paul.chernoch|Paul.chernoch]] ([[User talk:Paul.chernoch|talk]]) 18:58, 26 February 2016 (UTC)

: Unless other contributors object, I suggest you change the task description accordingly, before there are even more entries. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 23:14, 26 February 2016 (UTC)

==task requirement==
I've noticed that some entries don't use the Brent algorithm in finding a solution. 

Is it an intent that the solutions   ''must''   use the Brent algorithm?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:07, 26 February 2016 (UTC)

I would prefer that it use Brent, since that makes it easier for those who might benefit from the algorithm to make an apples-to-apples comparison when looking at different language implementations. At the very least the implementer should identify which algorithm they are using. The performance characteristics can vary considerably among algorithms.
--[[User:Paul.chernoch|Paul.chernoch]] ([[User talk:Paul.chernoch|talk]]) 22:52, 26 February 2016 (UTC)

: When performance is critical it's probably best to not use an algorithm which uses O(1) space. (On the other hand, performance is usually not critical.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:15, 3 March 2016 (UTC)
