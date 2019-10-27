+++
title = "Talk:Primality by trial division"
description = ""
date = 2014-09-14T09:13:49Z
aliases = []
[extra]
id = 2500
[taxonomies]
categories = []
tags = []
+++

== "simplest"? ==

The task states "Implement the simplest primality test, using trial division." I'd like to submit that any such statement is always inappropriate at this wiki, as it presumes knowledge what method of solving some task is "the simplest" <I>in all conceivable languages</I>. There may well be languages where some other method might be "simpler", especially given that "simple" is not exactly a particularly well-defined term. In particular in the sense of complexity defined through the computational resources required to express the result, a Miller-test might well be "simpler" than trial-factoring.[[User:Sgeier|Sgeier]] 13:22, 4 February 2008 (MST)

:Why not just say "Use trial division to test each number"? --[[User:Mwn3d|Mwn3d]] 15:19, 4 February 2008 (MST)

:: Even that isn't particularly precise. Should I do trial-division by every integer 2 through n-1? Or am I allowed to factor only up to (sqrt(n)) which is obviously sufficient but introduces the added complexity of invoking a square root. So is it still the "simplest" solution? Is simplicity preferred? Actually it would be entirely sufficient to trial-factor only by all <i>prime</i> numbers up to sqrt(n) which immediately leads to a very elegant recursive implementation. Which I'm pretty sure isn't what the creator of the task had in mind ;-) but which is entirely allowed when you just say "use trial division"... [[User:Sgeier|Sgeier]] 19:21, 4 February 2008 (MST)

::: It isn't necessary to invoke (use) a SQRT function to find the square root to limit the factors.  See the REXX example. [[User:Gerard Schildberger|Gerard Schildberger]] 23:06, 18 December 2010 (UTC)

::: Also, trial division by three's can be skipped.  See the REXX example and others. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:12, 15 May 2012 (UTC)

==new description==
I fixed up the description. I tried to be more specific, but didn't rule out recursion. If you use recursion make sure you note it and add the task to the [[:Category:Recursion|recursion category]] --[[User:Mwn3d|Mwn3d]] 19:46, 4 February 2008 (MST)

== Python ==

This task can also be handled by the [http://tnt.math.se.tmu.ac.jp/nzmath/ NZMATH] modules prime and arith1 to enable its trialDivision function.----[[User:Billymac00|Billymac00]] 02:42, 3 January 2011 (UTC)
: As I noted in [[Talk:Sieve of Eratosthenes]], feel free to add a code example, as long as there's already an example that satisfies the task description as-written. --[[User:Short Circuit|Michael Mol]] 04:19, 3 January 2011 (UTC)

== Extending the task to producing ranges of primes, optionally. ==

I propose the task description is amended (what is the correct procedure for this?).

The following to be added: 

"Optionally, use trial division to produce sequences of prime numbers."

For the context to this, see [[Talk:Sieve of Eratosthenes#Trial division sieves should be moved from here|Sieve of Eratosthenes talk page]]. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 16:36, 11 September 2014 (UTC)

:* the new proposal on the [[Talk:Sieve of Eratosthenes#Trial division sieves should be moved from here|Sieve of Eratosthenes talk page]] is '''''instead''''' to '''create the new page titled "Generating sequence of primes by Trial Division"'''.

::* [[Sequence of primes by Trial Division|Done and done]]. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 09:09, 14 September 2014 (UTC)
