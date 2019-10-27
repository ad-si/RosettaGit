+++
title = "Talk:Pascal matrix generation"
description = ""
date = 2016-03-27T22:36:26Z
aliases = []
[extra]
id = 19135
[taxonomies]
categories = []
tags = []
+++


### task clarification


When this Rosetta Code task asked     ''··· to write functions ···''     it seemed to imply to write a function for each form of the Pascal matrices.   Is the coding of a singular function to generate all three forms of Pascal matrices acceptable? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:07, 16 May 2015 (UTC)

:Individual functions was my intention. Is there really that much to gain by having one function parametrised with  the output required?
:I guess if there is documented language style for having one parameterised function in a similar area then it might be good to reference the standard library function(s) that you copying the style of then have the one function, but three functions is what the task expects. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:37, 17 May 2015 (UTC)

:: Should I remove the 2<sup>nd</sup> REXX entry then?   ... and will you be marking other entries as "needing improvement" or somesuch?   The 2<sup>nd</sup> Rexx entry uses a singular function to generate all three forms of Pascal matrices.   However, in its defense, it does read better.   But, a lot of whitespace added to the 1<sup>st</sup> REXX version would add a significant vertical whitespace. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:14, 17 May 2015 (UTC)

::: Well, I started that (after my sloppy reading of the task specs). My function<b>s</b> are pascal & comb although comb is, strictly speaking, not a <b>function</b> :-) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:35, 17 May 2015 (UTC)

:How about you add a note? I guess the focus should be on getting comparable entries. What do others think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:53, 17 May 2015 (UTC)

:: A note was added to the 2<sup>nd</sup> REXX entry.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:36, 27 March 2016 (UTC)


### range of numbers


Because the Pascal symmetric matrix uses     <big>'''i+j'''</big>     in the computation of the binomial coefficients (combinations), the calculation of the factorials can exceed the (default) maximum amount for some computer programming languages (number types) unless specified otherwise.   I found that with REXX's default digits of   '''9''',   it lead to generating numbers that weren't integers   (close, but no cigar)   with a matrix size of   '''11'''.   It would be interesting to know what the (practical) limitations of each language entry is, if any   (as far as generating exact integers). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:07, 16 May 2015 (UTC)

:If you use the following instead of the factorial version then you can reduce the onset of overflow problems: <math>\frac{n(n-1)(n-2)\ldots(n-k+1)}{k(k-1)(k-2)\ldots 1}</math>
:By calculating the numerator and denominator and comparing them separately to 2<sup>x</sup> you could explore when overflow might affect the calculation. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:50, 17 May 2015 (UTC)

:: The above formula is indeed the method that the REXX versions use, plus a refinement to optimize using the smaller factorial products. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:07, 17 May 2015 (UTC)

:::I thought you might have but was too lazy to check but then thought it might be good to leave the comment for others so left it. (Such poor justification for laziness, I know).
:::--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:58, 17 May 2015 (UTC)

:::: Yes, computing the factorial product using the smaller numbers is indeed quite a bit faster.   Of course, it only makes a noticeable difference if a large number of factorial products are being computed, or if the numbers being used are on the gihugeic side.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:30, 27 March 2016 (UTC)
