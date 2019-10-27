+++
title = "Talk:Numerical integration"
description = ""
date = 2019-07-04T20:45:05Z
aliases = []
[extra]
id = 2430
[taxonomies]
categories = []
tags = []
+++

==Java's trap function==
The Trap function in the Java example does not call F(X). Is this an error? --[[User:Waldorf|Waldorf]] 14:21, 21 December 2007 (MST)
:Good catch. I'm working on it now. --[[User:Mwn3d|Mwn3d]] 14:29, 21 December 2007 (MST)

==Parser note (C)==
Writing the C code, I've used the var name If (Integrated function, in my mind), and noticed the syntax highlighter highlights If as '''if'''! It should be case sensitive! --[[User:ShinTakezou|ShinTakezou]] 23:38, 16 December 2008 (UTC)
==Copying Bad Code==
It seems a lot of people copied the Java and Ada entries without actually thinking about the code or testing it.  The right rectangle examples were leaving out the final rectangle; this was originally probably a copy/paste error from the left-rectangle, which does want to stop 1 h early.  The mid rectangle routines were all just averaging the beginning and endpoint of each rectangle rather than sampling the middle.  This is essentially recreating trapezium.  In some cases, trapezium is also wrong, because while you count the endpoints only once, each interior function is used twice, and should be multiplied by 2.  These particular entries were not doing that.  Testing would have found most of these problems.
--[[User:TimToady|TimToady]] 07:51, 11 September 2010 (UTC)
:IIRC I copied the C example to make the Java example. I had used the C example for a homework assignment in college on these algorithms. I'd like a little more explanation on these problems. Maybe some pseudocode? You understand my resistance since I probably spent a week or two talking about these integration rules in a class with a reputable professor at my college. It is highly possible that I still didn't code them right, though. --[[User:Mwn3d|Mwn3d]] 16:19, 11 September 2010 (UTC)
::A correct implementation in C of the mid rect can be seen at [[http://en.wikipedia.org/wiki/Rectangle_method]].  The important thing to notice is that the interval (h in most of the rosettacode examples) is divided in half to find the middle point at which to evaluate the function (the i * 0.5 in the middle).  If you're not dividing the interval in half, you cannot determine the value of the function at that point.  Averaging the beginning and ending function values of the rectangle is the same number only if the function is linear, because doing that (and multiplying by the width) is simply another way of calculating the area of a right trapezoid.  For good "pseudo code" that isn't so pseudo, I suggest the Algol 68 example.  Notice how it has h/2 in the mid rect to get to the middle of the rectangle.  The trapezium is careful to weight the inner points twice as much as the endpoints.  (But don't copy the right rect, I think it's wrong to subtract h from the end position, because we already added h to the start position, and that leaves out one of the rectangles.  Maybe use the Common Lisp one for that, since it gets it right.) --[[User:TimToady|TimToady]] 03:18, 12 September 2010 (UTC)
:::On closer inspection, the Algol code was wrong too, so I just fixed it so you'd have something to look at.  But you really need to learn to visualize the geometry of it, I think.--[[User:TimToady|TimToady]] 03:40, 12 September 2010 (UTC)
::::Aknowledged... C code going to be fixed (maybe...!) --[[User:ShinTakezou|ShinTakezou]] 18:29, 14 November 2010 (UTC)

== f(x) = x cases. ==

My rationale for the two f(x) = x cases were threefold:
# I wanted a simple case that would expose accuracy differences between the approximation methods across function types, and a function with a linear slope would do that. (At least when compared to x^3 and 1/x).
# I wanted a case that would be relatively easy to get an exact answer by hand
# I wanted to see if I could expose differences in floating-point/rational number implementations between languages using a simple case.
Roughly speaking, IEEE 754 floating-point numbers (what most programmers are probably accustomed to working with) allow for up to 24 bits of integer precision for a 32-bit floats. This means they will tend to precisely represent non-negative integers of up to 2^24, or 16,777,216. (I'm sure it's slightly more complicated than that, or I would have mentioned it along with the test cases. In any case, a simple test program I wrote for incrementing a float detected integer precision loss when it tried to go past 2^24 or thereabouts.) The final result of the [0,5000] case is an integer below 2^24, and so it can be accurately and precisely represented. The final result of the [0,6000] case is an integer above 2^24, and may not be. (I'm sure there's someone who will read this who can identify the the next greater positive integer above 2^24 that can be represented with an IEEE 32-bit float, and could speak to this directly.) Granted, in either case, the intermediate values during approximation are likely to lose precision by always falling on exact representations. Using a different number of approximating steps (such as 5,000 and 6,000 respectively) could cover that. --[[User:Short Circuit|Michael Mol]] 08:03, 12 September 2010 (UTC)

:I needed several hours of computation to complete the answers to the last two tests in Python (but that's what sleep is for, right). Unfortunately Python uses double-precision FP by default so had no problem with precision. I would have thought that a more 'squiggly' function would be the best to show differences in approximation methods as several are probably exact when approximating a straight line. Maybe 2**24*sin(x) for 1<=x<=1 using 1000 steps might do the trick (without taking the chill off my study early in the morning). --[[User:Paddy3118|Paddy3118]] 08:41, 12 September 2010 (UTC)
:: Well, a linear function should also wind up exact, in the case of the trapezoidal function. (The integration from 0 to x for y = mx + b where b = 0 boils down to finding the area of a triangle.) I suppose using 32 steps each for [0,4096], [0,8092], [0,x = 67108864] and [0,268435456] would accomplish the same thing; the values correspond to:
::* For exploring 32-bit float precision, where the boundary is at 2^24
::** [0,4096] -- 2^23
::** [0,8092] -- 2^25
::* For exploring 64-bit float precision, where the boundary is at 2^53. I chose +/- 2 in the exact value's exponent because +/- 1's seed value value must be represented as a factor of sqrt(2).
::** [0,67108864] -- 2^51.
::** [0,268435456] -- 2^55.
::At only 32 steps each, these should take minimal time to compute, while poking the boundaries of 32-bit and 64-bit precision. (You could possibly reduce it to 2 or 4 steps, or even 1 step if you don't care a great deal about the precision of the other methods. (I could be way off here. I haven't actively applied calculus since I took Calc 2 in college about eight years ago. --[[User:Short Circuit|Michael Mol]] 15:31, 12 September 2010 (UTC)

Computation time didn't bother me, but I think the choice of n being up in the millions causes multiple digits of precision to be lost in accumulated representation error.  The result is that differences between methods are obscured by the common effect of accumulated representation error.  I agree, 32 would be a fine number.  So would 4, or 10, or even 1000. &mdash;[[User:Sonia|Sonia]] 22:40, 14 January 2011 (UTC)

== missing or incomplete output(s) ==
Has anyone else noticed that almost   '''1/2'''   of the programming examples don't have any output   or   have an incomplete output?   (Output should have twenty results).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:00, 28 June 2016 (UTC)

== the deletion of the   ''Common Lisp''   entry ==
Why was the   '''Common Lisp'''   programming language entry replaced with   '''Comal'''   with no explanation    (on July 3<sup>rd</sup>) ?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:55, 3 July 2019 (UTC)

Thanks, [[User:Thundergnat|Thundergnat]], for restoring the   '''Common Lisp'''   entry.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:42, 4 July 2019 (UTC)
