+++
title = "Talk:Map range"
description = ""
date = 2010-11-30T17:39:01Z
aliases = []
[extra]
id = 8857
[taxonomies]
categories = []
tags = []
+++

This task was created for one simple reason: I was trying to remember how to do what the C++ example I wrote and provided does. The task description can probably use work. I also know that there are weird scenarios, such as in resampling, where that code doesn't quite work, but I think that's a different problem. (Possibly worth its own task. Given two arrays of N1 and N2 elements, find the corresponding element in N2 for N1.) --[[User:Short Circuit|Michael Mol]] 16:17, 25 November 2010 (UTC)
: So we're talking about floating-point values? If we are, we can just ignore the nasties with sampling errors since they at least ''model'' reals. –[[User:Dkf|Donal Fellows]] 16:27, 25 November 2010 (UTC)
:: The particular case I recall occurred at work some time ago when trying to scale up drawing operation coordinates on a raster display. Not quite real life, sadly. If this task should renamed to 'map range of reals' to keep things simple(r), that'd be fine by me. (I wonder what that does to n-dimension and imaginary numbers, where n is greater than 1.) BTW, did you see my note on {{tmpl|uses from}}? --[[User:Short Circuit|Michael Mol]] 16:48, 25 November 2010 (UTC)
::: I did see them. For {{tmpl|tcllib}}, I made the target component be a string SMW property instead of a page (using the parameter to {{tmpl|property}} on the property's page), which makes sense anyway since I don't plan to make a page for every component package in Tcllib (too damn many; CPAN would have the identical problem). Or were you talking about something else and I've misunderstood? :-) –[[User:Dkf|Donal Fellows]] 22:15, 25 November 2010 (UTC)
:::: I got a similar maintenance reaction for Java. I wonder if there's a way to automate that kind of maintenance, or toggle the page/string option. Maybe {{tmpl|uses from}} should use the string property instead. --[[User:Short Circuit|Michael Mol]] 22:26, 25 November 2010 (UTC)

To be clear, this is also called [[wp:linear interpolation]]. --[[User:IanOsgood|IanOsgood]] 17:39, 30 November 2010 (UTC)

== Adjustments and related tasks ==

When I wrote this task, I was just looking to write the linear mapping function. I'm not sure that explicit test samples from the source and destination sets are appropriate; I think that will reduce the effectiveness of languages which are likely to graph their results, like Maple, Octave or Mathematica.--[[User:Short Circuit|Michael Mol]] 14:46, 26 November 2010 (UTC)

:Any idea of how to re-write this to allow graphing an answer? (I don't regularly use graphing tools). --[[User:Paddy3118|Paddy3118]] 16:43, 26 November 2010 (UTC)
:: It's just a kind of program output. The real problem I was touching on, I think, was that one couldn't demonstrate using a contiguous function. I loosened the task requirements again, so that should be doable. --[[User:Short Circuit|Michael Mol]] 22:17, 26 November 2010 (UTC)

I'd like to see a related task which performs a nonlinear mapping. (I hadn't thought of the nonlinear case when I wrote the task.) Specifically, an arbitrary one, where an <math>f(x)</math> can be provided to define the...distribution? For example, if <math>f(x)=x</math>, the numeric result would be the same as with this task. --[[User:Short Circuit|Michael Mol]] 14:46, 26 November 2010 (UTC)

: In other words, given maprange function g, and arbitrary function f, compute g(f(x))?  --[[User:Rdm|Rdm]] 14:59, 26 November 2010 (UTC)
:: I suppose so, yeah. That would handle the analogous cases I could think of. (converting a linear 16-bit PCM input to, e.g. 8-bit a-law or <math>\mu</math>-law.) --[[User:Short Circuit|Michael Mol]] 22:17, 26 November 2010 (UTC)
