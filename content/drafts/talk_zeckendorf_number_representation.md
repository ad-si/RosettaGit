+++
title = "Talk:Zeckendorf number representation"
description = ""
date = 2012-11-28T02:12:04Z
aliases = []
[extra]
id = 12372
[taxonomies]
categories = []
tags = []
+++

== Consensus on the sequence ==

Googling around, there seems to be a lack of consensus on the sequence of Fibonacci numbers used and how the sequence is indexed.  If you include both F(1) = 1 and F(2) = 1, then the representation 1 is not unique.  Similarly, if you include F(0) = 0, then the representation of 0 is not unique.

Mathworld's page on Zeckendorf's theorem mentions that it applies to {F-1}, that is, the Fibonacci sequence with one of the 1s removed, and presumably without the 0.  &mdash;[[User:Sonia|Sonia]] 22:39, 10 October 2012 (UTC)
: There simply can't be two 1s, otherwise a lot of numbers won't have unique summation: anything in for form of big_fib + 3 can also be written as big_fib + 2 + 1 (that's using the first one; the second one would be next to 2, if that makes sense -- well if it doesn't, it sort of proves the point also.) --[[User:Ledrug|Ledrug]] 01:27, 11 October 2012 (UTC)

:I will modify the task description to specify an algorithm to use that can only give a unique sequence. Thanks for the heads-up. --[[User:Paddy3118|Paddy3118]] 12:08, 11 October 2012 (UTC)

::Task specific 2 is still ambiguous for the case of ZR(1).  The sequences {1} and {1, 1} both satisfy the criterion of the greatest of the sequence being a number less than or equal to the number for conversion, but the greedy rule leads the first to produce ZR(1) = 1 and the second ZR(1) = 10. &mdash;[[User:Sonia|Sonia]] 17:51, 11 October 2012 (UTC)

This is me asking for help. Help!

Anyone care to suggest a rework of the task that would be better. Or would a re-wording allowing different starting conditions so long as they were stated suffice? Technically the task is still draft but it would be good if any change meant minor or no change to the existing solutions of others. P.S. Thanks Sonia, Ledrug, TimToady for the comments so far. --[[User:Paddy3118|Paddy3118]] 20:02, 11 October 2012 (UTC)

: The sequence should be specified as 1,2,3,5,8..., and a Zeckendorf representation of a non-negative integer n is n expressed as the sum of non-consecutive terms in that sequence.  This is sufficient and unambiguous: every n >= 0 has a unique such representation, and vice versa.  I don't think the task should specify ''how'' one derives such a summation from n; listing a method as a hint, fine, putting it in the spec as if it's required, no.  --[[User:Ledrug|Ledrug]] 23:47, 11 October 2012 (UTC)

:: I agree with these points.  I'll add that it seems most sites say Zeckendorf representation only specifies a sum, and does not specify the binary place value coding that turns 3 + 1 into 101.  I kind of like the binary coding but I think it might be good to describe it as an additional encoding on top of Zeckendorf representation.  [http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibrep.html#fibbase Dr Ron Knott's site] calls this the "Fibonacci base system."  Other links:
::* [http://mathworld.wolfram.com/ZeckendorfRepresentation.html Mathworld] hints at the binary encoding by using 0 or 1 as a multiplier of Fibonacci terms.
::* [http://mathworld.wolfram.com/ZeckendorfsTheorem.html Z's theorem at MathWorld]
::* [http://en.wikipedia.org/wiki/Zeckendorf%27s_theorem Wikipedia] is careful to say the theorem applies to ''distinct'' Fibonacci numbers (my emphasis.)
::* [http://oeis.org/A003714 OEIS A003714], "Fibbinary numbers".
:: &mdash;[[User:Sonia|Sonia]] 00:32, 12 October 2012 (UTC)
:: Oh wait, there's [http://oeis.org/A014417 OEIS A014417] "Representation of n in base of Fibonacci numbers" which is even better.  &mdash;[[User:Sonia|Sonia]] 00:39, 12 October 2012 (UTC)
:::I've rewritten the task to be based on distinct Fibonacci numbers, and the task now references the OEIS sequence as the desired result without specifying the algorithm.  I don't think it will be any great hardship to the current entries to switch to this approach, since it merely involves starting the Fibonacci sequence at a point where it leaves out the first 1.  --[[User:TimToady|TimToady]] 05:16, 12 October 2012 (UTC)
::::Okay, I think I've patched everything to be consistent with the new task description, but please double-check my work to see if I've done something stupid in your favorite language.  --[[User:TimToady|TimToady]] 06:27, 12 October 2012 (UTC)

:::::Much thanks guys, the fixes are great. I am currently trying not to get a full blown cold, which appears to be what's affecting my concentration. --[[User:Paddy3118|Paddy3118]] 18:35, 12 October 2012 (UTC)

: It should be noted that this task can be solved without using a Fibonacci sequence.  See the '''generic''' example in the REXX section. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:34, 23 October 2012 (UTC)

:: That doesn't make sense at all.  Try the sequence 1, 2, 4 ... 2^n, how do you make a representation that's garanteed to have no 11s? --[[User:Ledrug|Ledrug]] 23:13, 23 October 2012 (UTC)

::: I don't understand "try the sequence".  Do you want those numbers expressed as Zeckendorf numbers (instead of 0, 1, 2, 3, 4, 5, ..., 20)?  The '''generic''' REXX example doesn't ''use'' a sequence at all. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:22, 23 October 2012 (UTC)

:::: Never mind, I thought "solved without using a Fibonacci sequence" meant "using some other sequence in its place", which obviously was my misunderstanding. --[[User:Ledrug|Ledrug]] 00:29, 24 October 2012 (UTC)

: Also, the '''general''' REXX solution solves the task for '''N''' Zeckendorf numbers, which merely generates a Fibonacci sequence whose last numer is greater or equal to '''N'''  (instead of hard coding six Fibonacci numbers). I think it'd be a better task to list up to '''N''' Zeckendorf numbers, with 20 being the default. That way, no short cuts would be used in the examples and thereby hiding the limitations of the programs. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:34, 23 October 2012 (UTC)

==Perl 6, wrong fib sequence==
Could there also be a submission for Perl6 that used the sequence starting 1, 1, 2, ... The present example could be put second as an alternative, with the present description of how it differs from the task description if you like. --[[User:Paddy3118|Paddy3118]] 12:14, 11 October 2012 (UTC)
:If you're going to mandate the 1,1 sequence, then there's no point in having alternatives.  On the other hand, you could mandate that the initial sequence must be specified by the user, perhaps defaulting to 1,1.  But 0,1 and 1,2 are also sane, the latter because it eliminates the useless 0 on the end of every result, and the former because that's also a common way to define Fibonacci sequences.  The 1,2 version is also appropriate under the notion that you're just using all the positive Fibonacci numbers in order, not the sequence itself.  But as my grandmother-in-law used to say, this is all a tempest in a teabag.  <tt>:-)</tt>  --[[User:TimToady|TimToady]]
:By the way, according to your own criteria, the Python solutions are incorrect for 1, since using the 1,1 in the reversed order greedily should produce '10', not '1'.  You shouldn't have to special-case 1 like that. --17:59, 11 October 2012 (UTC)
::More to the point, if you don't introduce a special discontinuity like that at 1, then the 1,1 and 1,2 solutions are isomorphic modulo the presence or absence of a trailing 0.  Though I should probably not have said "incorrect" above; as Sonia points out, it's ambiguously specified.  But I'd very much prefer to disambiguate the rule by saying something like "Using all the numbers in the sequence less than or equal to the number."  This is in keeping with the greediness and the reversal of the sequence. Alternately I would not be adverse to going back to a formulation that uses "All the positive Fibonacci numbers in order."  That is, the 1,2 solution. Which most of the implementations chose back when it was still unspecified as 1,1... --[[User:TimToady|TimToady]] 18:11, 11 October 2012 (UTC)
== J wiki edit trouble ==
It's supposed to show as

```J

' '&~:Filter@:":@:#:@:#.@:((|. fib 2+i.8) e. fsum)&.>i.3 7
┌──────┬──────┬──────┬──────┬──────┬──────┬──────┐
│0     │1     │10    │100   │101   │1000  │1001  │
├──────┼──────┼──────┼──────┼──────┼──────┼──────┤
│1010  │10000 │10001 │10010 │10100 │10101 │100000│
├──────┼──────┼──────┼──────┼──────┼──────┼──────┤
│100001│100010│100100│100101│101000│101001│101010│
└──────┴──────┴──────┴──────┴──────┴──────┴──────┘

```

--LambertDW 02:12, 28 November 2012 (UTC)
