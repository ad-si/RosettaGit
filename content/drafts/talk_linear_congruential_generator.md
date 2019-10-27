+++
title = "Talk:Linear congruential generator"
description = ""
date = 2015-08-23T18:27:55Z
aliases = []
[extra]
id = 10012
[taxonomies]
categories = []
tags = []
+++

== OK, What's the task ==

Not to be impatient but so far this is a nice explanation of LCRNG's but there is no task.

There are other tasks with LCRNGs like [[Random_number_generator_(included)]] which pertains to what is built in.

What do we need to do to solve this task?  --[[User:Dgamey|Dgamey]] 01:06, 2 July 2011 (UTC)

: The task with built-in RNGs doesn't necessarily map. There's no guarantee that the built-in RNG is a linear congruential generator; that's just the most common implementation option. –[[User:Dkf|Donal Fellows]] 15:42, 2 July 2011 (UTC)
:: Still there is no task see "insert task here".  It's basically encyclopedic. --[[User:Dgamey|Dgamey]] 02:29, 3 July 2011 (UTC)

==On the task==
I kinda remember something about not all seeds being good? a seed of zero for example? --[[User:Paddy3118|Paddy3118]] 05:07, 7 July 2011 (UTC)
:Shouldn't be.  Because of the coprimality between a and m, all values from 0 to m-1 will appear exactly once in a cycle, so any seed is as good as another.  You may be thinking the situation where people choose ''predictable'' seeds, such as 0, current time, process id, etc while trying to use PRNG for sensitive work, giving an attacker a higher chance of success at finding the pseudo random sequence. --[[User:Ledrug|Ledrug]] 05:23, 7 July 2011 (UTC)
::Aha! Thanks Ledrug. I found what I was thinking of - I was thinking of [[wp:Linear feedback shift register|LFSR's]]. --[[User:Paddy3118|Paddy3118]] 06:52, 7 July 2011 (UTC)
:::There is a class of linear congruential generators where c = 0: the formula is <math>a * r_n \pmod m</math>. If the seed was zero, then these generators would yield zero, zero, zero, zero.... Today, I found that FreeBSD rand() uses such a formula:
:::*<math>r_{n + 1} = 7^5 \times r_n \pmod {2^{31} - 1}</math>
:::If the program tries <math>r_0 = 0</math>, then FreeBSD uses <math>r_0 = 123459876</math>. --[[User:Kernigh|Kernigh]] 04:23, 15 July 2011 (UTC)
::::Huh, how does that work?  <math>2^{31}-1</math> is a prime number, so no <math>r_n</math> will ever produce a 0 return value.  <code>RAND_MAX</code> presumably is <math>2^{31}-2</math>?  Aren't <code>rand()</code> supposed to return a value from 0 to <code>RAND_MAX</code> inclusive? --[[User:Ledrug|Ledrug]] 07:52, 15 July 2011 (UTC)
:::[http://svnweb.freebsd.org/base/head/lib/libc/stdlib/rand.c?revision=174541&view=markup Here is the FreeBSD code.] Also, [http://svnweb.freebsd.org/base/head/include/stdlib.h?revision=206997&view=markup RAND_MAX is <math>2^{31} - 1</math>]. --[[User:Kernigh|Kernigh]] 18:12, 15 July 2011 (UTC)

== Example Sequences==
It occurs to me that the BSD LCRNG can produce an overflow of 32 bit words.  It would be nice to have a sequence that exercises this to ensure the behavior is reproduced.   --[[User:Dgamey|Dgamey]] 04:58, 8 July 2011 (UTC)
: Overflow how? Intermediate results can be done with larger integer types. --[[User:Ledrug|Ledrug]] 06:18, 8 July 2011 (UTC)
:: Sure, but in the historical implementations they probably were not done that way.  In many similar systems the multiplication would exceed 2^31 and cause an integer overflow. It wouldn't produce an exception but you could suddenly be looking at a negative number. That may not yield the same sequence if you are working with large integers or 64 bit words. --[[User:Dgamey|Dgamey]] 12:28, 8 July 2011 (UTC)

::: If the seed is '''42''', then the first 5 numbers from BSD rand() are 1250496027, 1116302264, 1000676753, 1668674806, 908095735.

::: Overflow is not a problem, if you can do correct math mod 2**31. If you overflow a 32-bit integer, then the low 32 bits should be correct, so you can still take the low 31 bits as the random integer. If you overflow a 10-decimal-digit integer, then you would have a problem, because 10**10 is not a multiple of 2**31. --[[User:Kernigh|Kernigh]] 20:25, 8 July 2011 (UTC)
:::: That is sort of my point.  If you do this with 32 bit signed ints you will get a different value that you will in larger ints.  For example (BSD)
:::::  (32 bits)     0, 12345, 1406932606
:::::  (larger ints) 0, 12345, 1406938949
:::: Some implementations may need to address this. That's also why having a common example sequence (with an overflow) is needed. --[[User:Dgamey|Dgamey]] 03:13, 12 July 2011 (UTC)
::::: Are you sure? If your integer is at least 32 bit long (shorter obviously won't do), sign bit has an impact only on the sign of modulo operator.  Using 64 bits, no overflow can occur and the sequence is 12345, 1406932606..., you did something wrong with your second series.  Also, if you use 32 bit signed integer, 2**31 is going to be negative.  If you use modulo operator you may get a negative random number which is pretty obviously wrong, but if your language convention always returns a positive one, then it's actually correct.  Largely there's no subtle error in this method, overflow or not. (Should clarify: above is assuming fixed length, 2's compliment binary integer representation.  It's clearly not a problem if you have arbitrary precision integers) --[[User:Ledrug|Ledrug]] 03:33, 12 July 2011 (UTC)
:::::: Ooops, Seems I am cross-eyed.  Had it in my head as 2^31-1 modulus (a couple of other historical LCRNG  .... --[[User:Dgamey|Dgamey]] 03:47, 12 July 2011 (UTC)

== When modulus matches the first operand's sign ==

When I wrote this task, I forgot that some languages have [[Arithmetic/Integer|a modulus operator that matches the sign of the first operand]]. With these languages, <code>formula (mod m)</code> might be negative and outside my expected range of 0 to m - 1. I added this <nowiki>{{needs-review}}</nowiki> note to the F# example:

{{alertbox|#ffffd8|These generators can yield negative numbers because the modulus operator of F# matches the sign of the first operand. The generators must only yield numbers from 0 to m - 1.
----
Negative numbers from lcg.bsd are <s>congruential</s> congruent to the correct numbers: -740551042 is <s>congruential</s> congruent to 1406932606. Negative numbers from lcg.ms are off by one, because the division truncated to the wrong direction: -11529 is <s>congruential</s> congruent to 21239, but expected 21238.}}

I know of 2 workarounds: (1) use [[bitwise operations]] to clear the sign bit, or (2) check if <code>formula (mod) m</code> is negative, and if so, add <code>m</code>. We might want to clarify the task. --[[User:Kernigh|Kernigh]] 17:21, 1 August 2011 (UTC)

:And some languages have the denominator for the first operand, instead of the numerator.  But I believe your objection is against languages where modulus takes on the sign of the numerator rather than that of the denominator. --[[User:Rdm|Rdm]] 19:52, 1 August 2011 (UTC)

== Should the vectorized variant of the X86 Assembly LCG be included? ==

I recently read the instructions on this page more thoroughly. I initially believed that it was only necessary to create an LCG, not to replicate an existing implementation. The example posted under "Second example using AVX instructions." does not produce identical output to the Microsoft rand(). Should this example be removed completely? For now, I will add a comment to the vectorized implementation explaining that its output will differ from Microsoft rand(). [[User:Emjay|Emjay]] ([[User talk:Emjay|talk]]) 21:38, 22 August 2015 (UTC)

: I would mark it as incorrect, maybe someone can fix it. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:22, 23 August 2015 (UTC)

:: I did what you suggested, and am curious to see what the solution might be, as my understanding of mathematics is very limited. Thank you for the input, please feel free to offer any other suggestions; I am new to this site, and wiki editing in general. [[User:Emjay|Emjay]] ([[User talk:Emjay|talk]]) 18:27, 23 August 2015 (UTC)
