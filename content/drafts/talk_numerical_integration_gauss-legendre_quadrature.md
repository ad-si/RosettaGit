+++
title = "Talk:Numerical integration/Gauss-Legendre Quadrature"
description = ""
date = 2018-08-24T04:58:26Z
aliases = []
[extra]
id = 9822
[taxonomies]
categories = []
tags = []
+++

How can we tell if an implementation of this task is correct?  (Ok, if we copy the lisp code and get the same answers that would probably be correct, but that does not address all the abstractions raised in the task description.)  --[[User:Rdm|Rdm]] 13:12, 30 May 2011 (UTC)

: Analytically, this is true:
::<math>\int_{-3}^{3} e^x \, dx = e^3 - e^{-3} \approx 20.035749854819805</math>
: That last value is what I get when I compute it directly (using my platform's libc's implementation of the exponential function for double-precision IEEE arithmetic). A correct implementation ''should'' tend toward that value in this case as the order of the Legendre polynomial being used is increased, up to a limit where it no longer helps. I observe that with the Tcl code, where I've tested up to 13 it indeed improves, but beyond that it ceases to help, becoming slow but getting no more accurate. I guess that this is running into the limits of the accuracy of the method for this type of equation (and note that it is delivering about 9 decimal digits of accuracy at that point; pretty good…) –[[User:Dkf|Donal Fellows]] 09:31, 2 June 2011 (UTC)

::My OCaml implementation converges to the correct value for n > 14 (see the example), but that may just be luck. [[User:TobyK|TobyK]] 19:04, 13 July 2011 (UTC)

How did you decide on the number of Newton-Raphson iterations to use when computing the node values? Wouldn't that also affect the accuracy? (I passed the required precision as a parameter and iterated until two consecutive estimates agreed up to that precision.) Exact floating point representations of the coefficients are also possible with sufficient precision. Increasing the precision always seems to give more accurate results, but having too many nodes can strangely make it worse. --[[User:Sluggo|Sluggo]]

: This is a two-fold problem.  In principal, the higher order you go, the better you can decompose your function into a sum of polymials, so with infinite order you can get exact result -- on paper.  In a computer, every time you do math with a floating point number, you get an error due to precision (up to <math>\sim 10^{-16}</math> of the value with IEEE 64 bit in general), and later you use the result to do more math, the error propagates and gets larger and larger.  Higher orders require a lot more arithmetic operations (about <math>O(n^2)</math> I think), so at some point, the precision error dominates and more terms only make results worse. --[[User:Ledrug|Ledrug]] 07:39, 28 June 2011 (UTC)

:: Though, of course, you could use something other than ieee floating point to represent your numbers -- perhaps arbitrary precision rationals? --[[User:Rdm|Rdm]] 11:06, 28 June 2011 (UTC)

::: Yeah but 1) Legendre polynomial roots are not always rational; 2) it's sloooow, if you go down that route, you might as well use some other way to do integrals. --[[User:Ledrug|Ledrug]] 14:32, 28 June 2011 (UTC)

:::: Idle musing...say you've got a scenario where your root may not be rational. If greater precision in the results may be needed later, but not now, one might save off the polynominal factoring routine's state for resumption later, no? I'm not saying it's necessarily useful for this task, just thinking about scenarios where irrational numbers in intermediate calculations might have their precision preserved. --[[User:Short Circuit|Michael Mol]] 15:35, 28 June 2011 (UTC)

::::: Hmm, not necessarily.  For polys higher than degree 5, there are no general analytical way to get roots, so all you have left is numerical solutions.  You could try to use rationals during <i>that</i>, for example while using Newton's method, but often there are so many iterations, your rational representation will grow too fast to be of much realistic use. --[[User:Ledrug|Ledrug]] 15:44, 28 June 2011 (UTC)

:::::: Two things. (1) a numerical solution is of limited precision, and in practical use, often only a limited precision is necessary. In a particular use case, some signaling of a desire for greater precision would allow for further calculation as-needed (a lazy evaluation, I guess). (2) Would it be possible for those intermediate routine states to still be useful for comparison? ("is {routine state} equal to {routine state}? Does a prior or projectable state indicate equality?") Admittedly, my line of thinking is towards usefully representing irrational numbers internally and exactly by way of incomplete calculation, and using the state of that calculation's routine as a value in other operations. --[[User:Short Circuit|Michael Mol]] 16:21, 28 June 2011 (UTC)

::::(unindent)  It's difficult to talk about feasibility abstractly.  You could represent var x simply as "root of that function" without fully evaluating it, and later logic may simply eliminate that function, hence the root, altogether, then good, lots of work saved; but if you are forced to take a value at some point, it then all depends on how you are going to use that value: you need to make decision now on precision desired, which can be [[wp:Wilkinson's_polynomial|surprisingly non-trivial]].  Stuff may be theoretically possible, but it's quite different from being practical, alas. --[[User:Ledrug|Ledrug]] 16:40, 28 June 2011 (UTC)

::::::: In the REXX examples, when using more decimal digits of ''pi'' and ''e'',   I found the limit of decimal precision is simply determined by how many digits are specified.   Of course, this slows up computation.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:55, 24 August 2018 (UTC)

== Tables and layout ==
Sorry for breaking the narrative flow (and introducing a bunch of other grammar and style issues) with the tables, but the previous interleaved format was visually unparseable for me. Having scraped through Calc 2 years ago, I could follow the math, but only once I saw it in this form, not the form that it had been in. If I have time, and if people don't have too strong an objective to this layout, I'll come back later and try to do more cleanup. --[[User:Short Circuit|Michael Mol]] 15:29, 28 June 2011 (UTC)

== Wikipedia content ==

Anyone know if the original page content came largely from WP? I'm not certain one way or another without digging into early revisions and doing text comparison, but if it was, we need citations to avoid copyright concerns. --[[User:Short Circuit|Michael Mol]] 15:30, 28 June 2011 (UTC)

: No, I've written everything myself, using WP and other articles merely as sources. --[[User:Avi|Avi]] 18:29, 13 July 2011 (UTC)
