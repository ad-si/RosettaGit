+++
title = "Talk:Roots of a function"
description = ""
date = 2008-06-10T05:06:33Z
aliases = []
[extra]
id = 2620
[taxonomies]
categories = []
tags = []
+++

I think that traditionally root finding algorithms have a very small difference value defined (called "epsilon" when I learned it) where if abs(f(x)) < this difference, then x is considered "close enough to a root." This is usually related to some sort of [http://en.wikipedia.org/wiki/Root_finding#Specific_algorithms named root finding algorithm] like bisection, regula falsi, or Newton's method (he has too many methods). Maybe this task could be edited (or other tasks made) to include those methods (I can give C code or at least pseudocode for some). --[[User:Mwn3d|Mwn3d]] 18:59, 21 February 2008 (MST)
:Feel free to change it. :-) --[[User:Short Circuit|Short Circuit]] 21:30, 21 February 2008 (MST)

It would also be interesting to include a symbolic math package which would use algebra to find the ''exact'' roots. --[[User:IanOsgood|IanOsgood]] 09:52, 22 February 2008 (MST)
::Your wish is my comand ;-) (I had wanted to include Maple for a while now, just never got around to it...) [[User:Sgeier|Sgeier]] 13:43, 12 March 2008 (MDT)
:::In your Maple example, is there any indication of exact-vs-approximation? (That part of the task description seems somewhat bothersome.) --[[User:TBH|TBH]] 16:54, 12 March 2008 (MDT)
::::hope that claarifies it...[[User:Sgeier|Sgeier]] 14:25, 13 March 2008 (MDT)

== Python example ==

In the Python example, I think it would be better to check if the value of f(x) were within a range including zero (an error bound of sorts) rather than exactly equal to zero. Sometimes computers aren't too good with getting things that exact. --[[User:Mwn3d|Mwn3d]] 23:06, 9 June 2008 (MDT)
