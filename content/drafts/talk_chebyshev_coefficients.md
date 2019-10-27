+++
title = "Talk:Chebyshev coefficients"
description = ""
date = 2016-10-09T20:40:16Z
aliases = []
[extra]
id = 19523
[taxonomies]
categories = []
tags = []
+++

== C example is copyrighted ==

As documented (!) the code is straight from NR in C and should be removed. &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 15:57, 21 August 2015 (UTC)

== Verify correct output. ==

All examples other than C currently appear derived from C, which has no output.  The J example has output for a test case and other examples have reproduced J's result, but some reassurance that the answer is correct would be nice.  Numerical Recipes in C goes on to provide Chebyshev evaluation function.  The task would be much more meaningful if the evaluation function were required also, with required output showing that the coefficents and evaluation function do accurately approximate some values of the original function.

Alternatively, task authors could code the evaluation function, run a test case through it, verify that it all works and then add the specific coefficents to the task description as required output.

Or alternatively, Clenshaw's recurrence might be added as a separate task, with the two tasks exchanging data and results.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 15:56, 21 August 2015 (UTC)

== Wikipedia doesn't seem to agree with Numerical Recipes ==

See https://en.wikipedia.org/wiki/Chebyshev_polynomials#Example_1.  WP has that extra Kronecker delta that effectively divides the first term by 2, which, if I understand, gets the absolute value of that first coefficient <= 1.  Am I reading that right?  NR may have a pair of functions that generate and then evaluate an approximating polynomial, but if it's a variant of Chebychev, I'd prefer that we show the more mathematically correct functions.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 16:15, 21 August 2015 (UTC)

== What is this task about? ==
It's absolutely not clear what '''Chebyshev coefficients''' actually are. The name of Chebyshev is associated to many things in approximation theory

* Coefficients of Chebyshev polynomials of first and second kind
* Compute the projection of a function on the Chebyshev basis, with scalar product defined by <math>(f|g)=\int_{-1}^{1}\dfrac{f(x)g(x)}{\sqrt{1-x^2}}\;\mathrm{d}x</math>.
* Given a polynomial in the basis <math>\{x^n,n\in\Bbb{N}\}</math>, rewrite it in the Chebyshev basis, leading to ''Chebyshev economization''.
* Find the optimal approximation of a function on a given interval. This problem is related to the ''Chebyshev equioscillation theorem''.
* Find the interpolating polynomial of a function at ''Chebyshev nodes'' (roots of Chebyshev polynomials), which often leads to a better approximation than equispaced nodes.

The task has to be clarified.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 15:14, 9 October 2016 (UTC)
