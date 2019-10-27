+++
title = "Talk:AKS test for primes"
description = ""
date = 2016-09-25T16:37:15Z
aliases = []
[extra]
id = 17176
[taxonomies]
categories = []
tags = []
+++

==Combinations==
There is a link between the coefficients of the expansion of <math>(x-1)^p</math> and nCr. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:56, 6 February 2014 (UTC)

==Lower Limit==
Again... 1 appears as an "is it prime, isn't it prime" candidate.

<math>1</math> is prime if all the coefficients of the polynomial expansion of: <math>(x-1)^p - (x^p - 1)</math>
are divisible by <math>p</math>. All the coefficients is the empty set. 1 is divisible by absolutely everything in the empty set. Is there a better wording out there? (That doesn't include the phrases "any number not 1" or "any number > 1") [[User:Tim-brown|Tim-brown]]

It could also be worded that there isn't any number in the empty set that is divisible by <math>1</math>. I think 1 should simply be ignored [[User:Denommus|Denommus]]

:If you can "sweep it under the carpet" then why not? It is a small part of the task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:34, 7 February 2014 (UTC)

: It would't be the first time a primality test has had some weird edge cases. (What about negative numbers? They'd need infinite polynomial expansions…) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 21:55, 7 February 2014 (UTC)

== This isn't AKS ==

This seems like a corollary to Fermat's Little Theorem, not the AKS test:

    (X+A)^N mod N = (X^N+A) mod N
    (X-1)^N = X^N-1 mod N    [set A=-1]
    (X-1)^N - (X^n-1) = 0 mod N

The youtube video calls this the AKS test, but it has little resemblance to the v6 AKS algorithm.  According to [http://en.wikipedia.org/wiki/Aks_primality_test#Concepts Wikipedia's AKS page], this method is exponential time.  It is Lemma 2.1 from the v6 paper -- just the leading off point for starting the AKS theorems and algorithm.  Edit: This is theorems 75 and 76 of Hardy and Wright, which was in the 1975 version of the 4th edition, if not in earlier editions.  It appears on page 58 of "The Elements of the theory of algebraic numbers" by L.W. Reid, 1910.  Clearly it is not AKS.

The time growth for an AKS implementation should be ~3-4x longer for each 10x input size increase.  Testing 99999989 should be somewhere in the range of 10 seconds for C code implementing the algorithm from the v6 paper.  This is of course all different by implementation, but this should give some clue.

:: ''Should be ... 10 seconds''   might be wishful thinking.   The problem is not the CPU time used but the amount of (virtual) memory that is (will be) used.   For the REXX test for 2,211, the biggest coefficient number computed was 668 decimal digits.   Larger numbers caused the exhausting of virtual memory (the REXX that I use is limited to around 2 Gbytes). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:55, 26 August 2014 (UTC)

::: That's because "the algorithm from the v6 paper" referenced above is completely different from what this task implements.  I can list at least one implementation of the v6 algorithm that take 10-20k and ~10 seconds to do this, and never use a number larger than a native int (32 or 64 bit).  As an aside, using AKS improvements gets it down to under 1k and a few milliseconds.  Eventually AKS does start taking large amounts of memory, but the improvements help a lot and overall speed becomes the bottleneck.  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 19:10, 26 August 2014 (UTC)

An aside, I'm a bit confused by their talk, especially at the end, that "this is a fast test".  It's ''ridiculously'' slow.  Nobody uses this test in practice.  It's very important in the theory, as it is deterministically polynomial time.  But the constants are horrendously large.  See, for example, [http://maths-people.anu.edu.au/~brent/pd/UMS10t4.pdf Brent 2010] "AKS is not a practical algorithm.  ECPP is much faster."  This includes run times of hundreds of years for numbers that ECPP does in a few seconds. [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 16:27, 21 February 2014 (UTC)

:I guess this is the AKS test according to numberphile at the moment. It was the easy suitability of the explanation in forming an RC task that made me create it; I only added the other link afterwards. If you get numberphile to retract then I would be glad to change the task name. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:53, 21 February 2014 (UTC)

:: Danaj is correct -- this isn't AKS, and it isn't fast. My entry has a 'fast' version of this task which uses modular reductions but even that isn't fast at all.
:: As for the real version of AKS, the state of the theory advanced very far in the years following the AKS paper. The original version was polynomial time but too slow to use on any kind of reasonable numbers. Now the state-of-the-art "AKS-class test" (as these are being called) is almost competitive with ECPP. It's still true that no one uses this test in practice, but if someone spent the time to write an optimized implementation using the lowest heuristic exponent (slightly more than 4) and Bernstein's improvements on the constant factor, it should only a few times slower.
:: [[User:CRGreathouse|CRGreathouse]] ([[User talk:CRGreathouse|talk]]) 14:34, 21 February 2014 (UTC)

::: I think the task itself is great for RC.  My complaints are (1) calling it AKS (read the paper), and (2) perpetuating the idea that this is a current practical speed breakthrough.
::: CRGreathouse is correct that there have been huge improvements since the first paper.  Importantly, someone else may come up with another insight that further improves the performance.  We haven't even seen a good implementation of the improvements from Bernstein's 2006 paper (that I'm aware of).
:::Brent's implementation in the 2010 presentation I referenced includes Lenstra and at least one of Bernstein's improvements (it looks like this is not the 2006 k=4 randomized algorithm nor the additional improvements Bernstein's 2002 paper mentions, but just his first ''s/r'' reduction plus Lenstra's improvement), and still gets quite long times.  Bornemann's 2002 Pari implementation uses the Lentra, Voloch, and Bernstein improvements, and is quite small and runs much faster than many others, but is still pretty far off APR-CL / ECPP. Going to the best "AKS-class" algorithm plus basic improvements would seem like it could be competitive.  I don't think those fastest versions are deterministic however (unlikely to matter in practice).
::: [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 18:10, 21 February 2014 (UTC)

:::: I agree with you on both of your complaints. I also have not seen implementations of the advanced versions (which, as you say, are not deterministic). - [[User:CRGreathouse|CRGreathouse]] ([[User talk:CRGreathouse|talk]]) 15:57, 25 February 2014 (UTC)


==Formulae rendered invisible to most browsers by cosmetic edits at 22:42, 28 August 2016==

Under-tested cosmetic edits, made to the task page at 22:42, 28 August 2016, have left the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect the redundant spaces which were injected into &lt;math&gt; tags, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:48, 21 September 2016 (UTC)

: Repaired 25 September 2016 by [[User:WillNess|WillNess]] – Thanks ! – [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:36, 25 September 2016 (UTC)
