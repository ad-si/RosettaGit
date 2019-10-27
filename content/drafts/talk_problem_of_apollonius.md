+++
title = "Talk:Problem of Apollonius"
description = ""
date = 2013-07-26T11:58:59Z
aliases = []
[extra]
id = 7969
[taxonomies]
categories = []
tags = []
+++

Suggest inline/block quoting the problem and algebraic solutions. (With citations, of course) --[[User:Short Circuit|Michael Mol]] 17:34, 13 August 2010 (UTC)

== Meta error propagation ==

Here's the danger of everyone copying everyone else's code: how many of the examples can handle the following three circles? <code>x=0, y=2, r=1; x=0, y=-2, r=1; x=4, y=0, r=1</code> --[[User:Ledrug|Ledrug]] 23:38, 14 September 2011 (UTC)
:OK great. What do we do to fix it? --[[User:Mwn3d|Mwn3d]] 01:46, 15 September 2011 (UTC)
:: Problems which are simple translations are usually indications that the translator is unfamiliar with the algorithm and/or the target language. Perhaps rewrites (or audits) should be preferred. Perhaps translations should show up on the unimpl pages as ENAs? --[[User:Short Circuit|Michael Mol]] 02:20, 15 September 2011 (UTC)
::: The problem is a division by zero, specifically when v11 is 0.  AFAIK it's in all the examples.  There may be other problems, for instance I wouldn't be surprised if all the solutions fail to handle some degenerate cases.
::: On a not quite related note, I do think line-by-line translations should be discouraged.  Even if you have to pick up the algorithm from some existing examples, it's probably better to rewrite the code from semi-scratch, if only to make sure it's more idiomatic to your language.  As to a solution -- I have no idea. --[[User:Ledrug|Ledrug]] 02:34, 15 September 2011 (UTC)

In searching of a proper fix, I scribbled many circles on a piece of paper, and reached the conclusion: I don't want any part of it.  First off, the current problem of dividing by zero is just a minor annoyance.  There are two fundamental problems:
# Circles can't always be represented by <code>[(x, y), r]</code>. Even if we ignore [[wp:Special cases of Apollonius' problem|degenerate input circles]], one still has to deal with degenerate solution circles, e.g. <code>[(0, -2), 1], [(0, 0), 1], [(0, 2), 1]</code> has two solutions that are straight lines.
# Touching each circle internally or externally isn't enough to identify a unique solution. <code>[(0, -3), 2], [(0, 0), 1], [(0, 3), 2]</code> has no solution that contains all circles inside, but has two solutions that's outside every circle.  Essentially this involves whether solution is allowed to have negative radius: if it is, there's the risk of double counting; if not, there's the risk of missing solutions.

Besides the above, there are other special cases: <code>[(0, 0), 1], [(0, 0), 2], [(0, 0), 3]</code> has no solution; <code>[(0, 1), 1], [(0, 2), 2], [(0, 3), 3]</code> has infinitely many solutions, to name just two.  Someone once said, "programming is all about special cases."  Unfortunately, for Problem of Apollonius, there seem to be nothing ''but'' special cases. --[[User:Ledrug|Ledrug]]

:For what it's worth: the J implementation treats all these degenerate cases as having no solutions. --[[User:Rdm|Rdm]] 20:11, 15 September 2011 (UTC)

:: Wait, not even for <code>[(0, -3), 2], [(0, 0), 1], [(0, 3), 2]</code>? --20:29, 15 September 2011 (UTC)

::: Yes.  For that case, it starts at the 0,-3 point (which is the center of the first circle), finds that the excess volume is negative infinity, and so stops there.  But the error for 0,-3 is 112 which is larger than the acceptable error volume.  So it reports no result.  I suppose the question is: what criteria should be used to determine when simplex has completed, if it's not the presence of excess volume?  Alternatively, what value should be reported for this case?  And, why?  --[[User:Rdm|Rdm]] 20:54, 15 September 2011 (UTC)
:::: I don't quite understand your question... assuming it's about solutions of the three circles, two of them are <code>[(-4, 0), 3], [(4, 0), 3]</code>. --[[User:Ledrug|Ledrug]] 21:04, 15 September 2011 (UTC)
:::: Huh so I guess math/misc/amoeba is some kind of adaptive general equation solver?  If so then your comment makes sence, but it also makes the J solution sound inadequate. --[[User:Ledrug|Ledrug]] 21:12, 15 September 2011 (UTC)
::::: It's not clear to me why this should be any worse than languages which return "Not a Number" for <math>0 \div 0</math>. --[[User:Rdm|Rdm]] 12:36, 16 September 2011 (UTC)
:::::: But we all seem to agree that other solutions are incorrect (I haven't looked at the recent D edit yet): being no more incorrect than other incorrect solutions doesn't say much, I think.  --[[User:Ledrug|Ledrug]] 19:30, 16 September 2011 (UTC)
::::::: My comparison was not between "J solution" and "Incorrect solution implemented in language X".  My comparison was between "J solution" and "languages such as Javascript which yield NaN for 0/0".  If the J solution is incorrect for giving a non-answer when there are an infinity of answers, then the Javascript language must also be incorrect.  --[[User:Rdm|Rdm]] 19:50, 16 September 2011 (UTC)
:::::::: <code>[(0, -3), 2], [(0, 0), 1], [(0, 3), 2]</code> doesn't have infinite number of answers.  Nor does it have an answer involving an inf: there are 8 solutions, all are finite circles. --[[User:Ledrug|Ledrug]] 20:12, 16 September 2011 (UTC)
::::::::: Oh, I misunderstood then.  Ok, yes, looking at the J solution, it explicitly checks for the cases where all circles are interior tangent and logically, something like <code>;(_1^#:i.8) <@apollonius 0  _3 2, 0 0 1,: 0 3 2</code> should treat all the cases, but that's not working for me, and I am going to have to do some debugging to figure out why.  (Note, by the way, that I did not write the J implementation here.) --[[User:Rdm|Rdm]] 20:27, 16 September 2011 (UTC)
::::::::: If I turn the <code>while.</code> to <code>whilst.</code> in math/misc/amoeba, then: 
```j
   ;(_1^#:i.8) <@apollonius 0  _3 2, 0 0 1,: 0 3 2
0 0 1
```
  I'll have to talk with Henry Rich to see if he agrees that this is a good change. Thanks! --[[User:Rdm|Rdm]] 20:31, 16 September 2011 (UTC)

==Turbines==
In general there are eight solutions to this problem. [http://en.wikipedia.org/wiki/File:Apollonius8ColorMultiplyV2.svg see] for a picture showing the eight solutions for a configuration similar to the one depicted in the task description.

Circles are of course passé‎, in modern geometry they are replaced by an abstract object called a turbine. A turbine is made of modern points, which are like old fashioned points but have an added direction property. A turbine is the set of points which are equidistant from an origin point. If the points point towards the origin it looks like a turbine. If the points point at 90deg to the direction to the origin (tangential) it looks like a circle. If the origin is directed to a particular point then the structure is called a clock. If all the points are tangential in the same direction it is called a cycle.

If we say that two cycles touch only if their directions are the same at this point then if we replace the three circles with three cycles then the problem has a unique cycle as a solution. Of course there are eight ways to replace the three circles with three cycles each of the eight solutions (converted to a cycle) in the picture will solve one of these arrangements.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:46, 26 July 2013 (UTC)
