+++
title = "Talk:Fibonacci word"
description = ""
date = 2013-11-10T01:14:57Z
aliases = []
[extra]
id = 15227
[taxonomies]
categories = []
tags = []
+++

Uh, are you sure that any of the Fibonacci words actually have rep-strings?
: It would seem not. I have replaced rep string with repeated substring.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:09, 13 July 2013 (UTC)

I'm not sure that's much better.  I've modified the Icon/Unicon solution to print the lengths of the
longest repeated substring for the first 20 Fibonacci Words.  It looks like
the lengths of those substrings are going to get you in trouble (again) with
Them That Be.  (I stopped after 20 because the algorithm I use for locating
repeated substrings is pretty naive and slow - hmmm, maybe <i>that</i> should be a separate
RosettaCode task?*).

In fact, thinking about it, the longest repeating substring has got to be <i>at least</i>
length(fword(n-2)) for all n > 2.  [I'll postulate, with only empirical evidence, that
the longest repeated substring of fword(n) <i>is</i> fword(n-2).]
Even expressing in hexadecimal isn't going to help much.

:Plan C:
::I have found an interesting paper http://hal.archives-ouvertes.fr/docs/00/36/79/72/PDF/The_Fibonacci_word_fractal.pdf which suggests another interesting task [[Fibonacci word/fractal]].
::This paper constructs the Fibonacci word in a slightly different manner to the one I am used to, but which comes to the same thing but one iteration later. I have adopted this method for clarity when comparing rosetta code with the reference.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:23, 15 July 2013 (UTC)


== Why is the entropy 0.9594...? ==
As a matter of interest it might be worth mentioning that for the ''infinite'' Fibonacci word (0100101001001...), the relative frequency of <math>0</math> is <math>p = 1/\phi</math>, where <math>\phi</math> is the Golden Ratio ''(1+sqrt(5))/2'', and therefore the entropy is given exactly by the formula <math>h(p) = -p*log_{2}(p)-(1-p)*log_{2}(1-p)</math> (=0.9594187282227441991428630...).
