+++
title = "Talk:Detect division by zero"
description = ""
date = 2015-11-06T14:40:26Z
aliases = []
[extra]
id = 7560
[taxonomies]
categories = []
tags = []
+++

Many of the solutions here simply check that the result is infinite.  This will fail if the numerator is 0 too, since 0 / 0 is mathematically incalculable (many languages return NaN here).

A correct pseudocode solution is:

 result = numerator / denominator
 if numerator equals 0
    if result is not a number
       divide by zero action
    end
 else 
    if result is infinite
       divide by zero action
    end
 end

: 0 / 0 is NOT mathematically incalculable -- it is trivially calculable.  The problem with 0 / 0 is that any numerical answer is a valid answer.  In other words NaN is not a valid result for 0 / 0 but is a description of the character of those answers.  (The result can be any of an infinite variety of numbers and not just "a" single number.)  This is a problem in mathematics because the result, by itself, is not sufficient to prove anything.  Thus, we at times use limits and other constructs to reason about cases involving 0 / 0.  A less deceptive result than NaN for 0 / 0 would be "Any Number", but to my knowledge no languages implement that.  --[[User:Rdm|Rdm]] 18:19, 18 June 2010 (UTC)
::Maybe it's correct as multiple numbers are '''N'''ot '''a''' '''N'''umber :-)
 (Being pedantic seems to be catching). --[[User:Paddy3118|Paddy3118]] 18:37, 18 June 2010 (UTC)
::: "In other words NaN is not a valid result for 0 / 0 but is a description of the character of those answers." --[[User:Rdm|Rdm]] 19:17, 18 June 2010 (UTC)
: I don't know what "mathematically incalculable" means, but I can testify that in pure mathematics, division by zero, regardless of the dividend, is undefined. Expressions such as <math>\lim_{x \to 0} \frac{x^2}{x}</math>, though they may appear to involve division by zero, actually don't. This expression, for instance, means "the number ''y'' such that for all positive ε there exists a positive δ such that for all <math>x \in (-\delta, \delta)</math>, <math>\left|\frac{x^2}{x} - y\right| < \epsilon</math>", i.e., 0. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 01:04, 19 June 2010 (UTC)
:: According to the [http://en.wikipedia.org/wiki/Defined_and_undefined wikipedia] "In mathematics, defined and undefined are used to explain whether or not expressions have meaningful, sensible, and unambiguous values. Whether an expression has a meaningful value depends on the context of the expression. For example the value of 4 − 5 is undefined if a positive integer result is required."  And the problem with 0/0 is the cardinality of the result, not the lack of any results. --[[User:Rdm|Rdm]] 11:37, 19 June 2010 (UTC)
::: I don't know of any conventional mathematical context in which "<math>\frac{1}{0}</math>" or "<math>\frac{0}{0}</math>" has a meaningful, sensible, and unambiguous value. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 18:42, 19 June 2010 (UTC)
:::: I thought 1/0 was often accepted to represent an infinity, though I can agree that infinities are not necessarily sensible nor unambiguous.  That said, mathematics seems capable of dealing with such subjects.  --[[User:Rdm|Rdm]] 02:58, 20 June 2010 (UTC)
:::: Limits are such a context. <math>\lim_{n\rightarrow+0}\frac 10</math> ''converges to'' (positive) infinity. It's not ''actually'' a value per se. When performing operations and calculations this if often not done precisely (by handling the limits) but for the most part that doesn't really matter. Still, ∞ isn't part of the real numbers. —[[User:Hypftier|Johannes Rössel]] 09:27, 20 June 2010 (UTC)
::::: Ok, but everything in mathematics is based on context, though of course the interesting math is relevant in a wide variety of contexts.  Nevertheless, we also have areas of mathematics where an operation logically could give any of an infinite set of results and for convenience we pick one of those results as the  for that expression.  One might argue that having the value of 0 for the principle value for 0/0 is not useful, but in my opinion this is akin to saying that 0 itself is not useful.  Note, for example, that this approach would usually eliminate the need for knuth's "strong zero" approach at http://arxiv.org/PS_cache/math/pdf/9205/9205211v1.pdf  Of course, exceptions can be constructed but they can be dealt with in much the same way as any other case that requires the use of a non-principal value.  --[[User:Rdm|Rdm]] 15:02, 2 July 2010 (UTC)
== Choices in Javascript Implementation ==
: Can someone provide reasoning for the way JavaScript solution is implemented? To me it looks like it is simply passing back the JS result and returning 0 if the expression evaluates to NaN. It also return 0 for divByZero(4,'n'). I think we can all agree diving by a string is not 0 :). I wrote a solution but don't want to add it in case I'm not completely understanding the function :) My solution 
    function divzero(l,r) {
        return ((l/r).toString() === 'Infinity' || (l/r).toString()  === '-Infinity' || (l/r).toString() === 'NaN') ? "error" : l/r
    }
Tested Chrome & Firefox, The methods used are in ECMA since JS 2.0. Tested with 4/2 return 2, 4/0 returns error, 4/-0 return error, 0,0 returns error, 0,n returns error, 0,4 returns 0
--[[User:Nycigor|Nycigor]] ([[User talk:Nycigor|talk]]) 00:02, 6 November 2015 (UTC)

:: The task asks that division by zero be detected, but leaves open what to do at that point. The decision to return zero should be seen not as detection but as an (arbitrary but silly) decision about what to do after detection. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:40, 6 November 2015 (UTC)
