+++
title = "Talk:Sum of a series"
description = ""
date = 2016-11-05T22:51:35Z
aliases = []
[extra]
id = 12625
[taxonomies]
categories = []
tags = []
+++

==Wrong name?==
Strictly speaking (from a mathematical point of view), this task is asking for the "sum of a sequence"; the term "series" refers instead to the list of partial sums of such a sequence, and you're not summing the series, because it's already summed <i>en passant</i>.  You can google for "series vs sequence" for more on this.  While "series" and "sequence" are often confused in common parlance, I don't see much harm in changing this task to a more accurate name from the viewpoint of the mathematicians. (I guess I'm a bit sensitive on this subject because at one point I had to go through all the Perl 6 docs and change all occurrences of "series operator" to "sequence operator".) --[[User:TimToady|TimToady]] 18:37, 22 November 2012 (UTC)
:Agreed.  I rephrased the intro accordingly.  Hope it's ok.  Maybe the title should be changed as well, but the current one is not so bad, providing a little justification is given in the intro.--[[User:Grondilu|Grondilu]] 19:08, 22 November 2012 (UTC)

:You could move [[Sum of a series]] to [[Sum of a sequence]] and have [[Sum of a series]] redirect to it to cover all bases? --[[User:Paddy3118|Paddy3118]] 20:24, 22 November 2012 (UTC)

::I think the title should have "series" in it because that's what we're talking about.  Maybe simply just "Series".--[[User:Grondilu|Grondilu]] 21:18, 22 November 2012 (UTC)

:So the task is actually to "''calculate a series''" as the mathematical definition of series has to include the notion of a sum of terms, whereas a sequence is just the ordered terms. --[[User:Paddy3118|Paddy3118]] 22:43, 22 November 2012 (UTC)
::Yeah but don't use that as a title.  No article on RC begins with "''Calculate a ...''".  For instance, [[factorial]] is just ''factorial'', not ''Calculate the factorial of a number''.  So if we really want to change the title of this article, I suggest just ''Series''.--[[User:Grondilu|Grondilu]] 23:22, 22 November 2012 (UTC)

My $0.02 worth. The task is actually to find the 'Partial Sum' of the sequence 1/k**2 between limits, which may be called a Finite Series. The Sum of a sequence (Series) is the sum over all values of an infinite sequence, and obviously can not be calculated just by adding them all up in a loop. --[[User:Nigel Galloway|Nigel Galloway]] 12:40, 23 November 2012 (UTC)

==summing backwards==

Has anyone experimented with summing the series ''backwards''?  That is, the least significant fractions first?

With REXX, summing the smallest fractions first yields a ''very slightly'' larger value, but only in the last few digits (3 or 4).

With 60, 80, or 100 digits of precision (or more), it seemed insignificant to me, almost like noise digits. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:39, 25 November 2012 (UTC)

:The last digit is different indeed.  Don't know why.  An other mystery of floating point arithmetic I guess.--[[User:Grondilu|Grondilu]] 05:24, 26 November 2012 (UTC)

==significant digits==

Could it be that for '''10<sup>n</sup>''' terms,
the significant number of digits (base 10) increase at every
'''10<sup>n</sup>''' and also at every '''[π * 10<sup>n</sup>]''' as '''n→∞''' ?

Possibly I mean as '''n''' gets very large. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:59, 25 November 2012 (UTC)

:The n-th term is <math>1/n^2</math>.  That's <math>10^{-2\ln(n)/\ln(10)}</math>, so with n big enough, you only add about 2n decimals when you add 10<sup>n</sup> terms.   It's not efficient at all, if that's what you thought.--[[User:Grondilu|Grondilu]] 05:15, 26 November 2012 (UTC)

:: No, I didn't think it was efficient all all.  I was musing about how often the number of (accurate) decimal digit increase. -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:26, 26 November 2012 (UTC)

::: REXX: should /*use D digits (9 is the default)*/ be changed to /*use D digits (60 is the default)*/ --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:15, 31 May 2013 (UTC)

:::: I'll make the comment to mean what I say.   I meant the default for REXX is 9 digits;   60 is the default for the program. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:39, 31 May 2013 (UTC)

== Wrong name (2014 edition) ==


The current title "sum of a series" nags me a little bit.  Can we change it into just "Series"?--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 12:20, 9 May 2014 (UTC)

== formulaic version ==

Does the formulaic version (as a '''Rust''' entry)  actually work for this series?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:51, 20 November 2015 (UTC)

Actually, I can't understand how the '''Rust''' version works (as per the 2<sup>nd</sup> calculation).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:53, 20 November 2015 (UTC)


==Main formulae rendered invisible to many browsers by cosmetic edits==

Cosmetic edits made to the task page at 19:32, 5 May 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the main formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:06, 20 September 2016 (UTC)

: Visibility of server-side formula graphics now repaired [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:51, 5 November 2016 (UTC)
