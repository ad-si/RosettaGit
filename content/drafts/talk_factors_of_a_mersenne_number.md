+++
title = "Talk:Factors of a Mersenne number"
description = ""
date = 2010-02-06T15:06:58Z
aliases = []
[extra]
id = 3310
[taxonomies]
categories = []
tags = []
+++

== Algorithm incorrect? ==
In testing some other Mersenne numbers, I get false negatives from the Python program.  Both M<sub>59</sub> and M<sub>6007</sub> are not prime and should yield a factor, right? If this is expected, maybe some more clarification is needed in the description. --[[User:IanOsgood|IanOsgood]] 19:28, 16 January 2009 (UTC)
:I think the arbitrary limit of (16384 / p) is too small for those cases. I found a factor for M<sub>59</sub> by increasing the limit. But I don't know how to determine what is a good limit to use. In the worst case we can just use sqrt(M<sub>p</sub>) as the limit I guess. That will make testing the actual prime cases take really long. --[[User:Spoon!|Spoon!]] 19:59, 16 January 2009 (UTC)

"q must be prime."
It doesn't need to be prime. If you are searching for Mersenne divisors it is efficient only to search for prime factors - but it doesn't need to be.
In fact this ModPow can be used for Probable Prime tests.

"This method only works for Mersenne numbers where P is prime (M27 yields no factors)."
It also works for composite exponents. Even the example you gave will find the factor 73 of M27, but 73 is really a factor of M9 and M27/M9 is indeed prime. But in general it does find real factors of composite exponent Mersennes.

Mathematica has a very efficient ModPow built in.

Paul Landon
