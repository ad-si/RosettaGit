+++
title = "Talk:Modular exponentiation"
description = ""
date = 2017-01-21T22:20:52Z
aliases = []
[extra]
id = 11083
[taxonomies]
categories = []
tags = []
+++

== Draft change ==
About 21 hours after I posted this draft, I changed the task from "last 20" to "last 40 decimal digits". I adjusted some solutions to print 40 digits, but I did not change the Go or Java programs. --[[User:Kernigh|Kernigh]] 19:49, 19 December 2011 (UTC)

== What if m < 0? ==

My draft currently specifies m &ne; 0. The answer when m < 0 might be ill-defined or inconsistent. Should I change from m &ne; 0 to m > 0 here? --[[User:Kernigh|Kernigh]] 23:18, 20 December 2011 (UTC)
: You could specify them all to be positive integers without causing problems; that's the only case that anyone actually wants in practice, and the others get into arguments (due to differing interpretations of how to extend the modulus operation to non-positive-valued domains). –[[User:Dkf|Donal Fellows]] 02:00, 21 December 2011 (UTC)

== Tell algorithm? ==

What's with needs improvement on the Java solution?  A number of other solutions here just call a library function.  In general, library algorithms might not always be known, or may change without notice so I'm not a big fan of telling library algorithms.  As with so many other tasks, this one could go in different directions:  Allowing simple library solutions, disallowing simple library solutions, or requiring a specific algorithm.  The [[wp:Modular_exponentiation#Right-to-left_binary_method|binary]] algorithm is an important one.  It wouldn't be so bad to require an implementation of that specific algorithm. &mdash;[[User:Sonia|Sonia]] 18:19, 24 December 2011 (UTC)
:I put the needs improvement on my own solution. I thought I would be able to figure out which alorithm it uses but I couldn't. I also thought other languages would have scratch implementations of specific algorithms so it would be nice to know which one it uses. You can take it off if you don't like it. --[[User:Mwn3d|Mwn3d]] 19:37, 24 December 2011 (UTC)
:: I suspect that all practical implementations use the “Right-to-left binary method” (the Tcl version certainly does) as that is clearly much more efficient than the others listed on the WP page for large exponents (which is what you need for the RSA encryption algorithm, by far the most important consumer of this function). Or they use a “left-to-right” method which is actually equivalent but consumes the bits in reverse order. –[[User:Dkf|Donal Fellows]] 20:35, 24 December 2011 (UTC)
I find Kernigh's notes on the OpenJDK algorithm interesting!  I'd be in favor of adding an extra credit task to provide a description like this. &mdash;[[User:Sonia|Sonia]] 18:40, 25 December 2011 (UTC)

==task requirements==

Did the task's author really mean that the algorithm must work with   '''any'''   integers    <big><math> a, b, m </math></big>   where   <big><math> b \ge 0 </math></big>   and   <big><math> m>0 </math></big> ?

I re-worked the REXX example to try to accommodate the task's requirements,   but nobody else even bothered to address their language's limitation on this issue.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:55, 14 May 2012 (UTC)

==Formulae hidden to most browsers by under-tested cosmetic edits at 04:42, 26 April 2016 ==

Under-tested cosmetic edits made to the task page at 04:42, 26 April 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the main task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:12, 22 September 2016 (UTC)

: Now repaired – visibility of task description formulae restored. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:44, 21 November 2016 (UTC)

==Apply Number Theory==

Since we're computing mod 10^40 here, we can reduce a modulo 10^40 right off the bat. And since 10 = 2*5, we can reduce b modulo (10^40)*(1-1/2)*(1-1/5) [https://en.wikipedia.org/wiki/Euler's_totient_function Euler's Totient]. Thus a = 8819466320163312926952423791023078876139 and b = 3690094744975233415544072992656881240319 is equivalent to the original specific question.
