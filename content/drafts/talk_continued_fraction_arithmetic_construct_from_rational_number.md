+++
title = "Talk:Continued fraction/Arithmetic/Construct from rational number"
description = ""
date = 2013-05-01T11:30:47Z
aliases = []
[extra]
id = 12895
[taxonomies]
categories = []
tags = []
+++

==Using r2cf as Generator a in [[Continued fraction]]==
Note that [[Continued fraction]] assumes Generators a and b are infinite. You may have to adjust [[Continued fraction]] to allow for r2cf terminating. The generator for sqrt2 may be used with r2cf.--[[User:Nigel Galloway|Nigel Galloway]] 13:17, 4 February 2013 (UTC)
:Sorry I may have missed something but what does r2cf stand for?--[[User:Grondilu|Grondilu]] 17:28, 4 February 2013 (UTC)
:: From the links, I'd guess “rational to continued fraction”. I think this task needs some work on naming! “<tt>Continued fraction arithmetic/Construct from rational number</tt>” would be a first guess; opinions? –[[User:Dkf|Donal Fellows]] 09:38, 5 February 2013 (UTC)
::: I would like Continue fraction arithmetic to be renamed “<tt>Arithmetic</tt>” as a child of “<tt>Continued fraction</tt>” so the final result would be “<tt>Continued fraction/Arithmetic/Construct from rational number</tt>”--[[User:Nigel Galloway|Nigel Galloway]] 12:56, 5 February 2013 (UTC)
:::: Not a problem at all. Won't move things until we've agreed exactly what we're going to though; too messy in terms of the redirects left behind… –[[User:Dkf|Donal Fellows]] 14:27, 5 February 2013 (UTC)

: BTW, Nigel, please use the internal link style for links within Rosetta Code (and <tt>wp:</tt> links for links into english Wikipedia) as that makes for better SEO. It's a small thing, but it's better for this site. Cheers! –[[User:Dkf|Donal Fellows]] 10:01, 5 February 2013 (UTC)
:: IAWTC--[[User:Nigel Galloway|Nigel Galloway]] 13:04, 5 February 2013 (UTC)

::: I've done the first parts of the rename. Any suggestion for what to call the other two pages? Perhaps “<tt>Continued fraction/Arithmetic/Monadic operations</tt>” and “<tt>Continued fraction/Arithmetic/Dyadic operations</tt>”? (Justification: one's a CF->CF operator system, and the other is a CFxCF->CF operator system.) But I'm not very attached to those names. –[[User:Dkf|Donal Fellows]] 11:54, 11 March 2013 (UTC)
:::: Gower uses Bivarate solution for arithmatic with two continued fractions. By extension the case with one continued fraction could be Monovarate. --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:30, 1 May 2013 (UTC)

== Titles are messed up ==


All CF-related articles should be under a generic "Continued fraction/" section.  For instance, this article should be:  "Continued fraction/r2cf" or "Continued fraction/From a Rational"--[[User:Grondilu|Grondilu]] 13:20, 5 February 2013 (UTC)

== Why just rationals? ==

It seems that the continued fraction of a number just depends on the value of that number. We don't actually need to know the information of numerator and denominator separately; we just need to know the value of the fraction they form. So instead of having a function that takes the numerator and denominator separately, we can have a function that takes one real number parameter, and for rationals we can simply divide the numerator by the denominator and pass it to this function. In fact, this can work for all real numbers, not just rationals, but also irrationals; the only difference is that the continued fraction for rationals terminate.

Now, with some numeric representations (e.g. floating point binary), not all rationals can be represented exactly. But actually this doesn't matter so much, since the continued fraction for two numbers that are close together will yield continued fractions that start out the same. So even an approximation will be right in the beginning (up until you see a very large number in the sequence, which you know is caused by a small error in the approximation).

I demonstrated this in the Python solution. I have an additional function that just takes one argument, a real number, and generates a continued fraction of it. I tested it on sqrt(2) (actually an approximation of sqrt(2), since it is floating-point), and it produces the correct beginning of the sequence. I also tested it on a rational (using an exact fraction data type), and it produces the correct result for a rational, matching the result from the function for rationals above.

So perhaps we can just generalize this to a function that just takes a real number. -- [[User:Spoon!|Spoon!]] 22:17, 10 February 2013 (UTC)

: Obviously it is not a problem to add other translations and explore their advantages and limitaions, noting that the results will be language dependant. Note that [[Continued fraction arithmetic/Continued fraction r2cf(Rational N)#1.2F2_3_23.2F8_13.2F11_22.2F7]] calculates the continued fraction for 22/7 as a rational number. [[Continued fraction arithmetic/Continued fraction r2cf(Rational N)#Real_approximations_of_a_rational_number]] demonstrates ever closer approximations of 22/7. The related continued fraction tasks will assume that you generate [3;7] when 22/7 is entered. If you enter 22.0/7 into your Python 'pseudo real' r2cf will you have to do more to interpret your result as [3;7,<math>\infty</math>] which is equivalent? Note that the Perl6 example handles 22/7 correctly, but Perl6 has built into it a Symbolic Maths ability. Compare [[Carmichael 3 strong pseudoprimes, or Miller Rabin's nemesis#Perl_6]] with [[Carmichael 3 strong pseudoprimes, or Miller Rabin's nemesis#Python]] and decide if this comes at a price or if Perl6's use of floor is a mistake. The objective in further tasks will be to calculate <math>\sqrt 2 \times \sqrt 2</math> as 2, which will require understanding the continued fractions as described here. --[[User:Nigel Galloway|Nigel Galloway]] 13:53, 11 February 2013 (UTC)

== Strange series ==
I've just been implementing this and I was looking at the series of numbers leading up to: <math>[314285714;100000000]</math> and I was wondering just what the point of it was. <math>[3141592653589793;1000000000000000]</math> is far more interesting! –[[User:Dkf|Donal Fellows]] 11:42, 10 March 2013 (UTC)
:Never mind me; you're trying for approximations to 22/7 and not actually π. D'oh! –[[User:Dkf|Donal Fellows]] 09:32, 11 March 2013 (UTC)
