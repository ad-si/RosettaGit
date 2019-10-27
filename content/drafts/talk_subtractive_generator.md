+++
title = "Talk:Subtractive generator"
description = ""
date = 2016-11-22T22:22:30Z
aliases = []
[extra]
id = 10203
[taxonomies]
categories = []
tags = []
+++

== Task Documentation error ==
The formula is stated as 
* <math>r_n = r_{(n - i)} - r_{(n - j)} \pmod m</math>
It should be 
* <math>r_n = (r_{(n - i)} - r_{(n - j)}) \pmod m</math>
Also, the addition of the modulus for <math>r_n < 0</math> is buried in the text. (Oops forgot sig yesterday --[[User:Dgamey|Dgamey]] 06:01, 5 August 2011 (UTC))

:Neither is a big deal.  mod is normally assumed to have a very low precedence, watch how TeX spaces this: <math>a-b\mod m</math>.  And mod is most often assumed to operate on a positive m and produce a positive result, so <math>r_n</math> should be positive after the modulus anyway. --[[User:Ledrug|Ledrug]] 01:17, 5 August 2011 (UTC)
:: Not sure about that C, Java, Icon, and others have mod operators with the same precedence as multiplication and division. Also, several of these will return negative numbers. Anyway it is just clearer to document it with extra (). --[[User:Dgamey|Dgamey]] 06:01, 5 August 2011 (UTC)

:::I first wrote the formula as
:::* <math>r_n = r_{n - i} - r_{n - j} \pmod m</math>
:::but I feared that readers would confuse it with
:::* <math>r_n = r_n - i - r_n - j \pmod m</math>
:::so I added some extra parens. Dgamey suggested to add yet more parens.

:::''mod'' is not really an operator. The formula is a ''congruence'' that says the left side and the right side would have equal remainders. For example, if we had
:::* <math>r_n = 39 - 9 \pmod 100</math>
::: then <math>r_n</math> can be 30, or -70, or 130, or 42000030, because all of those integers are congruent (mod 100) to 39 - 9. In the task, I buried the phrase "uniform random integers from 0 to m - 1". This was my attempt to pick from the many congruent integers. --[[User:Kernigh|Kernigh]] 03:34, 7 August 2011 (UTC)

== So... what am I doing wrong? ==

I've implemented this algorithm, including Bentley's cleverness, and it just does not look right to me.

For example:  i= 55, j=24, m= 1e9, picking a random seed: 109488485

This gives me an initial sequence that looks something like this: 109488485 1 890511516 890511515 999999999 109488484 109488485 1 890511516 890511515 999999999  ... and when I re-order it and run the generator for an extra 165 iterations S looks like this: 275363168 101452672 275363168 275363169 275363169 1 1 724636832 724636832 724636833 724636831 449273662 999999998 999999998

In other words, the resulting sequence has some extremely predictable artifacts.  And using J=21 instead of J=24 might improve the situation slightly (I got 337354485 325291032 674708968 325291034 674708966 325291034 999999998 2 ... for my initial S, picking a different random S0), but I still see the artifacts.

And the artifacts seem to have a life of their own, independent of my <math>\scriptstyle S_0</math>.

Is the algorithm really this bad?  Or am I doing something wrong?  --[[User:Rdm|Rdm]] 15:14, 2 August 2011 (UTC)
: Did you get the sequence as in example with seed 292929?  The following code
: <s>code scrubbed</s>
: gave me different answers. --[[User:Ledrug|Ledrug]] 17:32, 2 August 2011 (UTC)
:EDIT I looked at Bentley's C code, and the task description is incorrect.  After step 4, the sequence r0 ... r54 needs to be reversed.  Bentley's code performs step 3 forward, but generates rand numbers by iterating the r array backward. --[[User:Ledrug|Ledrug]] 17:51, 2 August 2011 (UTC)

:::I found my problem.  I was using s[-1] - s[-2] instead of the other way around.  --[[User:Rdm|Rdm]] 19:06, 2 August 2011 (UTC)

:: I wrote this task. I thank Rdm and Ledrug for trying to solve the task. Ledrug found a mistake. My test code used r(n) = r(n + 55) + r(n + 24) (mod 55). In the task, I wrote r(n) = r(n - 55) + r(n - 24) (mod 55) and accidentally reversed the subscripts of r. I now '''edit step 4''', reversing the order of r0, ..., r54. --[[User:Kernigh|Kernigh]] 19:39, 2 August 2011 (UTC)
::: Eh wait, current description is still off by 1.  <math>r(34 n - 1\mod 55) = s(n)</math> is equiv to <math>s(34 (1 + n)\mod 55) = r(n)</math> for <math> n = 0\cdots 54</math> --[[User:Ledrug|Ledrug]] 19:54, 2 August 2011 (UTC)
::: While the introductory text uses i=55, j=24, the step by step breakdown seems to use i=55, j=21 and 34 just happens to be i-j.  And my implementation still does not quite work right, but maybe I should wait a bit for the task description to settle down. --[[User:Rdm|Rdm]] 19:58, 2 August 2011 (UTC)
:::: No no, 21 is used for shuffling state array during init, while 24 is used for generating random numbers, they are not the same number. --[[User:Ledrug|Ledrug]] 20:02, 2 August 2011 (UTC)
::::: Ok... but I do not see why 21 is being introduced.  Ok, sure, 21 and 55 are relatively prime and so are (55-21=34) and 55.  But the same holds 24 and 55 and (55-24=31) and 55.  This would not be such a big thing, but this seems to be being explained as a general concept as opposed to a cookbook technique where only the listed values are appropriate.  --[[User:Rdm|Rdm]] 22:13, 2 August 2011 (UTC)
:::::: Shuffling the state array doesn't seem to be necessary.  It's probably like the 165 extra generations, done to minimize the influence of the initial seed.  If so, it's probably better to use a number totally unrelated to the step used in generation (24) while not too big or too small to avoid possible correlations.  I'd guess instead of 21, 19/17/29 etc probably would be fine too, though 2 would be more questionable. Just a guess. --[[User:Ledrug|Ledrug]] 23:27, 2 August 2011 (UTC)

== I do not understand ==

I tried to write a python implementation based on the given algorithm (I see most of the samples claim to be translations of the C code). My implementation did not produce the same results as the C code, so I analyzed the C code to try to find my error, and I think I have found that the C code (and by extension all the code samples that are derived from it) is in error. I base this claim on the fact that the algorithm descriptions clearly states that r[54] = S[0] = seed and that r[33] = S[1] = 1, but when I look at r[55] in the C code I find it is 445406075, not 292929, and r[33] = 913647517, not 1. I inspected these values after the first for loop in subrand_seed.

Could someone please look into this and tell me whether: 
    I) the description of the algorithm is wrong, 
    B) the C code is wrong, or 
    3) (most probable) I have done something stupid.
-------------------------------
I figured it out, the array is backward I assumed r[n] in the description was the same as state[n] in the code, but r[n]= state[54-n].

I need to dig deeper to find why my code is wrong. ''--unsigned comment by [[User:EdK|EdK]]''

: Did you read the preceding discussion on this page?  --[[User:Rdm|Rdm]] 15:13, 4 January 2012 (UTC)

:: EdK has now posted a working Python program.
:: Any program that generates 467478574, 512932792, 539453717 from seed 292929 is probably correct. When I wrote this task, I used code from ''xpat2'' to generate these numbers. --[[User:Kernigh|Kernigh]] 17:32, 4 January 2012 (UTC)


==Nearly all formulae rendered invisible to many browsers by cosmetic edits==

Cosmetic edits made to the task page at 04:25, 12 September 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left nearly all of the formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:56, 20 September 2016 (UTC)

: Repaired – visibility of formulae restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:22, 22 November 2016 (UTC)
