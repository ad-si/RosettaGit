+++
title = "Talk:Averages/Arithmetic mean"
description = ""
date = 2012-05-08T01:29:15Z
aliases = []
[extra]
id = 4736
[taxonomies]
categories = []
tags = []
+++

Why is it required that an arbitrary result, 0, be returned when the input is empty? --[[User:Kevin Reid|Kevin Reid]] 22:57, 14 August 2009 (UTC)
:I suppose we have to define something. From the [[wp:arithmetic mean|Wikipedia page]], the mean is defined as "the sum from i=1 to n ...", so the mean of an empty set of data is undefined. The problem could have specified Inf or nil or anything. (However, it seems the [http://www.simplyhired.com/a/salary/search/q-Empty+Set average salary of the empty set] is US$24,000 :/ ) --[[User:Glennj|glennj]] 19:46, 27 August 2009 (UTC)
:I second this question.  The decision that an empty list should return a value of 0 is completely arbitary and lacks rationale.  It might as well return 6 or a googol or pi.  It would be better if it returned some undefined value or if the function definition were improved to specify the value that would be returned if the input array is empty. --[[User:Eliasen|Eliasen]] 08:11, 15 March 2012 (UTC)
:: Mean value of empty set is sort of a 0/0 problem. I suggest the "return zero" requirement be dropped.  Code could be allowed to throw an error, return something indicating undefinedness, or just do whatever it wants and call it "undefined behavior".  Any objections? --[[User:Ledrug|Ledrug]] 23:30, 29 April 2012 (UTC)

:Hi, I like the change that Ledrug has applied but the changed wording puts examples that followed the original wording at a disadvantage as they may well have a means of returning errors or undefined values but the task stated that zero be returned. How about this alternative:
::''"In case of a zero-length input, zero may be returned, but since the mean of an empty set of numbers is ill-defined, the program may choose to behave in any way it deems appropriate, and if the programming language has an established convention for conveying math errors or undefined values then this may also be followed."''
:--[[User:Paddy3118|Paddy3118]] 15:32, 7 May 2012 (UTC)

:: Well, that's kinda the point: average of nothing really is a mistake and should be flagged as such.  If it doesn't matter in your application, then any return value makes as much sense as another, so there's no need to single out 0 there--"any action you see fit" is a super set of "return 0" anyway.  Think it this way, what if the task says "you may return 42"? It's also fair because everyone would then be equally disadvantaged. --[[User:Ledrug|Ledrug]] 22:35, 7 May 2012 (UTC)


::: You're right. I'll leave the task description as-is. --[[User:Paddy3118|Paddy3118]] 01:29, 8 May 2012 (UTC)
