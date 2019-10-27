+++
title = "Talk:Sequence of non-squares"
description = ""
date = 2017-01-27T21:45:36Z
aliases = []
[extra]
id = 3016
[taxonomies]
categories = []
tags = []
+++

== chose a way to investigate functions ==
I chose this as a way to show how easy it is to investigate functions in a programming language. --[[User:Paddy3118|Paddy3118]] 08:42, 24 August 2008 (UTC)

== Stability, Accuracy ==
The formula need to be investigated for numeric stability. Calculation of sqrt is inexact, it need to be shown that for all n in question, floor(1/2 + sqrt(n)) yields the exact result. A minimal requirement for this (though insufficient, I guess) is that sqrt has an error below 0.5. Note that the addition following to squaring will normalize for big n. Also conversion of those to floating point becomes quickly inexact when 32-bit floats are used. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:51, 24 August 2008 (UTC)

:I suggest they choose/use the precision that gives correct results over the stated range, or reduce the range to suit the precision they have and add a comment. I think its true that most languages on Rosetta supporting floating point also support more than 32 bit FP values. --[[User:Paddy3118|Paddy3118]] 11:28, 24 August 2008 (UTC)

::Maybe it would be better to require an implementation not to use floating-point at all. I mean that m=floor(1/2 + sqrt(n)) should be solved iteratively using, say Newton method, in integers. That would guaranty that rounding error do not mangle the result. The case arises when sqrt(n) lands into the interval m.49999..m.50001. This is instable if the precision of floating-point numbers is 0.0001, because one cannot decide whether the floor of + 0.5 were m or m+1. Mathematically, for any given precision of floating-point numbers, there exist an infinite number of n, such that sqrt(n) is in the interval like above. There exist algorithms for integer sqrt(n) defined as the integer lower bound of real sqrt(n). One could modify them for 0.5+sqrt(n), that would be the cleanest way.

::I have just noticed that there is a typo error in the task name: "sequance" instead of "sequence". --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:30, 24 August 2008 (UTC)

:::Whoops! I have [http://www.rosettacode.org/wiki/User_talk:Spoon! asked]. --[[User:Paddy3118|Paddy3118]] 13:56, 24 August 2008 (UTC)

: The best way to avoid numerical instability is of course to remain integer all the time. My thoughts to this:
: Let's assume that k is the largest integer so that k^2 <= n. Then it's obvious that sqrt(n) = k + x, where 0 <= x < 1.
: Therefore:
:* floor(1/2 + sqrt(n)) = k if x < 1/2, i.e. n < (k + 1/2)^2 = k^2 + k + 1/4. Since n and k are integers, this simplifies to n <= k^2 + k = k(k+1).
:* floor(1/2 + sqrt(n)) = k+1 otherwise.
: Maintaining k is quite simple: Since our n grows one by one, we just have to increment k as soon as n==(k+1)^2. Looking again at it, one sees that this can be further optimized: Since we need k+1 as soon as we are larger than k(k+1), we can just use that condition (which we would have to check anyway) for incrementing k. Therefore the algorithm would go like this (untested)

```txt

n := 1, k := 1
proposition_is_right := true
while (n <= 1000000)
  result := n + k
  if (result is square)
    proposition_is_right := false
  n := n + 1
  if (n > k*(k+1))
    k := k + 1

```

: This doesn't use any floating point, and therefore isn't susceptible to rounding errors. Note that the largest number occuring in the calculation (namely k*(k+1)) isn't too much larger than n, so unless you get close to the upper end of your integer type range, you shouldn't get overflow. --[[User:Ce|Ce]] 15:33, 2 September 2008 (UTC)

:: You can rewrite it as n - k*k > k without risking integer overflow. Then k*k from the former step can be stored in order to reduce the number of multiplications down to O(sqrt(n)). So if this task is really about a sequence, i.e. n+floor(...) is not required to be programmed as a closed function, then this is looks like the most efficient solution. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:47, 2 September 2008 (UTC)

::: Well, using that (k+1)^2 = k^2 + 2k + 1, you can even completely eliminate all multiplications. However the stated goal of the task (see top of discussion page) was "to show how easy it is to investigate functions in a programming language." So while this algorithm is certainly a solution of the task (well, printing out some values is missing) avoiding the numeric problems, I'm not sure if it isn't against the idea of the task. Maybe a clarification of the task would be a good idea. --[[User:Ce|Ce]] 07:36, 3 September 2008 (UTC)
:::: Try and implement something '''close to''' the given function that is being investigated, at least at first. --[[User:Paddy3118|Paddy3118]] 18:03, 3 September 2008 (UTC)


:IEEE754 guarantees almost 7 decimal digits (6.9something iirc) for normal 32-bit floats and the last computer architecture I've ever heard of that used non-IEEE754 floats was the DEC VAX. Certainly anything x87-based should abide by that. Since log(2) is about 0.3, this means square roots should be stable and OK for this task up to about 10^6.6 or there abouts. Since we're only supposed to test a million numbers, I see no problem here anywhere (if someone feels like testing it, it should break down a little over 4 million for 32-bit floats, a little over 2e15 for 64-bit floats). Or am I missing something here somewhere?
[[User:Sgeier|Sgeier]] 21:02, 28 September 2009 (UTC)

See GAP implementation for a way to use integers all the way. Of course, it will be best suited to languages with bigints, like python or ocaml, but others will do as long as you don't go past MAXINT/100. You also need to implement integer square root if it's not available in your language (not too difficult). The main idea is Sqrt(100*n) = 10*Sqrt(n) (let's call it x), so you get the first decimal of Sqrt(n) as floor(x) mod 10, or simply x mod 10 if using integer square root.
[[User:Toucan|Toucan]] 08:27, 15 June 2011 (UTC)

== Zero ==
I thought we were in the realm of Number Theory where [http://en.wikipedia.org/wiki/Natural_number#History_of_natural_numbers_and_the_status_of_zero Wikipedia states] they don't include zero as a natural number, so went with the term Natural Number, but the term Positive Integer ''might'' be more exact. --[[User:Paddy3118|Paddy3118]] 11:36, 24 August 2008 (UTC)

== OCaml: superfluous truncate?  ==
In this function for OCaml:
  let nonsqr n = n + truncate (floor (0.5 +. sqrt (float n)));;
Why do you need truncate as well as floor? Oh wait, does that change a floating point number with no fractional part into an integer?
You may have guessed, I don't know OCaml. --[[User:Paddy3118|Paddy3118]] 07:38, 25 August 2008 (UTC)

==bc problem==
Doesn't follow the task. Should print the given, limited range, then assert the condition for a million. Not print a million. --[[User:Paddy3118|Paddy3118]] 13:05, 5 March 2009 (UTC)

: To Bc makes no sense since there's no limit. No "assert" checked needed. You can put 10^6 or 10^100 as you prefer... (only time and memory (for arbitrary precision) are the limit. --[[User:ShinTakezou|ShinTakezou]] 13:09, 5 March 2009 (UTC)

: Ah sorry, understood the sense now! Instead of reading the task, I've looked at sources, and get wrong understanding of the task, sorry! Going to fix it! --[[User:ShinTakezou|ShinTakezou]] 13:10, 5 March 2009 (UTC)

: Back on my steps: the assertion exists to check the validity of the formula upto 10^6, or to check the validity of the implementations (because of precision) upto 10^6? I think the formula has no such limit; so the check is there because of limits that bc, by design, has not... so to bc makes not too much sense the "assertion" part. Anyway, it is there now, if I've got it right this time! --[[User:ShinTakezou|ShinTakezou]] 13:27, 5 March 2009 (UTC)

== algorithm ==

Is there a reason that the task states (and most people implement) <tt>floor(1/2 + ...)</tt> instead of simply <tt>round(...)</tt>?
[[User:Sgeier|Sgeier]] 20:18, 28 September 2009 (UTC)

In some programming languages round may give different results. E. g. in Scheme: "Round returns the closest integer to x, ''rounding to even when x is halfway between two integers''. Rationale: Round rounds to even for consistency with the default rounding mode speciﬁed by the IEEE ﬂoating point standard." But since we are only rounding the square roots of integers, in this case we would not run into problems like these. Maybe it's because [http://en.wikipedia.org/wiki/Floor_function floor] has been well-defined in mathematics and [http://en.wikipedia.org/wiki/Rounding round] hasn't. [[Special:Contributions/131.155.116.18|131.155.116.18]] 17:09, 30 November 2009 (UTC)


== Ambiguity ==
The task only asks to check that there are no squares in the sequence, but then we don't check that there are all non-squares ! However, it's easy to keep track of "the next square to come", and check that we go through all integers up to that number, then compute the next square, etc. Easiest with a generator, but a loop will do.
[[User:Toucan|Toucan]] 08:27, 15 June 2011 (UTC)
:What's the difference between "there are no squares" and "they are all non-squares"? --[[User:Ledrug|Ledrug]] 18:53, 15 June 2011 (UTC)
::(if(a) then (b)) says nothing about conditions when (a) is not true. (if("There are no squares") then ("they are all non-squares")) is a true statement, so if a number is, it must either be a square or a non-square--but this makes no guarantee that there are any values at all. However, we dp get that guarantee from the task's asking for a range from [1,22].
::Since there are values, and since those values must either be squares or non-squares, then if we show that there are no squares in the sequence, the resulting sequence must consist of non-squares. I don't see an ambiguity with the task description, though perhaps the reasons the existing requirements are sufficient could be more clear. --[[User:Short Circuit|Michael Mol]] 20:07, 15 June 2011 (UTC)
:::Heh, ask a rhetorical question, get a supersized answer.  A nitpick though, an empty set S is normally considered to sastify the requirement "for every E in S, E is not an A".  Kinda how an empty set is considered to have a permutation of 1, come to think of it.  --[[User:Ledrug|Ledrug]] 20:29, 15 June 2011 (UTC)
::::What I wanted to say: if your sequence is u(n) = n^2 + 1, then there are no squares in the sequence (for n > 0), and by the task's proposed test, it would be accepted (I mean the second test, but it would be easy to build a list that is correct for its first 22 terms, then wrong). But not all non-squares are in the sequence. This is not a rhetorical question. [[User:Toucan|Toucan]] 03:42, 16 June 2011 (UTC)
:::::Ah, <i>that</i>'s what you meant, that the sequence should contain ALL non-squares.  I'm not sure if the task did ask that, maybe the author should clarify.  --[[User:Ledrug|Ledrug]] 05:01, 16 June 2011 (UTC)
::::::The task asks for the ''sequence of non-square natural numbers''. If in doubt, it even gives a link to OEIS sequence A000037. It's not really bad to check that there are no squares in this sequence, but it would be better to check that all non-squares are here. Since it's not difficult to do, there is no reason no to do it I think. [[User:Toucan|Toucan]] 06:14, 16 June 2011 (UTC)
:::::::(There is even a link explaining the natural numbers too). --[[User:Paddy3118|Paddy3118]] 06:28, 16 June 2011 (UTC)

:::::::With over fifty examples, it is too late to mess around with the task goals, although I probably did miss the chance to ensure that the non-square naturals are represented up to a limit. --[[User:Paddy3118|Paddy3118]] 06:28, 16 June 2011 (UTC)
::::::::Ok, no problem ;-) [[User:Toucan|Toucan]] 08:53, 16 June 2011 (UTC)
