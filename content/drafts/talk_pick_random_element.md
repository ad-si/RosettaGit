+++
title = "Talk:Pick random element"
description = ""
date = 2017-08-26T22:48:27Z
aliases = []
[extra]
id = 21577
[taxonomies]
categories = []
tags = []
+++


### task clarification

The task should clarify if the elements should be taken at random with equal probability (that is, discrete uniform distribution, insofar as the underlying pseudorandom generator allows this). It seems obvious, but it's not stated, and if so, there are wrong entries in the page.

For instance, the C program does not yield a uniform distribution. A correct solution is explained in the accepted answer to this Stack Overflow question: '''[https://stackoverflow.com/questions/2509679/how-to-generate-a-random-number-from-within-a-range How to generate a random number from within a range]'''. It describes a form of '''[https://en.wikipedia.org/wiki/Rejection_sampling rejection sampling]''', and one would expect a similar approach in a correct implementation. For instance, Python does this for the '''[https://github.com/python/cpython/blob/master/Lib/random.py random.randint]''' function.

The same bad C code is reused in the [[Loops/Break]] task, and I fear many could use the Rosetta Code program as is without suspecting a problem.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 22:26, 26 August 2017 (UTC)
