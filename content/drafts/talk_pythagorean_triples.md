+++
title = "Talk:Pythagorean triples"
description = ""
date = 2016-10-30T17:43:44Z
aliases = []
[extra]
id = 9981
[taxonomies]
categories = []
tags = []
+++

== Confirmation of coprime ==

So perimeter is really easy. Coprime isn't something that people outside of math talk about much. I had to look it up on [[wp:coprime|Wikipedia]] to figure it out, but I'm still not quite sure. It's defined nicely there for two numbers, but we are dealing with three here. Are a, b, and c coprime if gcd(a, b) = gcd(b, c) = gcd(a, c) = 1? Do I need the third gcd evaluation? --[[User:Mwn3d|Mwn3d]] 15:28, 28 June 2011 (UTC)
: Since a^2 + b^2 == c^2, if any two of them are coprime, the third is also coprime to both, so there's no difference how you interpret "coprime" in this situation. --[[User:Ledrug|Ledrug]] 15:50, 28 June 2011 (UTC)
::I see what you did there. So, in general, you need all three gcd's, but in this case (because of the relationship between a, b, and c) you only need two. --[[User:Mwn3d|Mwn3d]] 16:16, 28 June 2011 (UTC)
::: Slight correction: you only need one. --[[User:Ledrug|Ledrug]] 16:18, 28 June 2011 (UTC)

== For reference ==
The correct results:<lang>           10 0 0
          100 17 7
        1,000 325 70
       10,000 4858 703
      100,000 64741 7026
    1,000,000 808950 70229
   10,000,000 9706567 702309
  100,000,000 113236940 7023027
1,000,000,000 1294080089 70230484

```
 --[[User:Ledrug|Ledrug]] 18:27, 28 June 2011 (UTC)

Hmmm... I get 324 and 4857 for perimeters of 1000 and 10000 respectively. I see that the Python solution also gets 324 for a perimeter of 1000. Am I missing something?--[[User:Tikkanz|Tikkanz]] 23:59, 28 June 2011 (UTC)
:I got 325 for 1000 using Java. Here's the full list: http://pastebin.com/HK74uBCz --[[User:Mwn3d|Mwn3d]] 00:49, 29 June 2011 (UTC)
::Good, beat me to it. I was wondering if I should post the list here, guess not :) --[[User:Ledrug|Ledrug]] 00:52, 29 June 2011 (UTC)
:::I'm running 10K now. Even if it finishes before I get bored I don't think I'll put up the full list. It seems like too much. I'll add the count here in any case. --[[User:Mwn3d|Mwn3d]] 00:54, 29 June 2011 (UTC)
:::It finished immediately after I clicked "submit"... I got 4858 triples and 703 primitives. --[[User:Mwn3d|Mwn3d]] 00:55, 29 June 2011 (UTC)
::Thanks, was a simple case of using < rather than <=. The Python solution should probably be corrected too.--[[User:Tikkanz|Tikkanz]] 20:35, 29 June 2011 (UTC)
Ok the goal of the extra, as I stated in task, is not to brute force them.  100k will seriously get on your nerves.  Instead of searching all possible triples, look at the generation section in the WP article, and device a good algorithm to create them: it's a world of difference.  I'll post a solution if everyone gives up, but I really want to have a task where people are required to think a lot. --[[User:Ledrug|Ledrug]] 01:16, 29 June 2011 (UTC)
:Thinking is for Project Euler! Maybe I'll try soon anyway. --[[User:Mwn3d|Mwn3d]] 01:40, 29 June 2011 (UTC)
::Heh funny you mentioned PE, this task is very similar in spirit, if not a down right copycat (it probably is, since there are many puzzles on PE that deal with right triangles). --[[User:Ledrug|Ledrug]] 01:48, 29 June 2011 (UTC)

== J solution ==

The J code speed sounds pretty good, now only if I can tell what the hell is going on in that code... care to explain, Rdm? --[[User:Ledrug|Ledrug]] 03:55, 29 June 2011 (UTC)
:Ok... I'm using [[wp:Pythagorean_triple#Generating_a_triple|Euclid's formula]] and generating primitive triples.  In other words, for all positive integers m and n where <math>m \leq \sqrt perimeter</math> and m > n and m+n is odd, I find the triple a,b,c = ((m^2)-(n^2)),(2*m*n),((m^2)+(n^2)), and discard those where the sum is greater than the perimeter.  I then find all multiples of each primitive triple where the multiple fits within the perimeter. I'm also sorting the result.  I'm also going through some extra wasted motion (for example, I am currently discarding duplicates though I can't find any cases where that is necessary). --[[User:Rdm|Rdm]] 10:52, 29 June 2011 (UTC)
::Clever. Thanks for the answer. --[[User:Ledrug|Ledrug]] 21:16, 29 June 2011 (UTC)

== Also seen at ==
FYI - Pythagorean triples are aslo part of [[List_comprehensions]] and snuck into [[Amb]] --[[User:Dgamey|Dgamey]] 12:39, 29 June 2011 (UTC)
:I think it's OK to have this task because it allows for "whatever you want" solutions. If there are examples in other tasks that do something really close to this, they can be copied here, modified for this task's requirements, and annotated. -[[User:Mwn3d|Mwn3d]] 13:02, 29 June 2011 (UTC)
::Actually I don't think this task has much to do with list comprehensions at all.  It just happens that that task uses Pythagorean triples as examples, while this task cannot be efficiently solved with list comprehension -- or show me how. --[[User:Ledrug|Ledrug]] 21:35, 29 June 2011 (UTC)

== Algorithm and performance: simple analysis ==

Since this task was made to show importance of a good algorithm, it would be helpful to make the difference clear.  Here are my understanding of different methods:

0. Task does not call for storing the triples, so space requirement is what you need to find each, meaning stack space and temp storage depending on how your method and language deal with them.

1. Brute force search of all a, b, c values.  Most naive would be checking all three in the space of 1 to perimeter, resulting a <math>O(n^3)</math>, but that's low even for a brute force: given a and b, only one value of c needs to be chcked.  This is <math>O(n^2)</math>, plus the complexity of testing square number and gcd; gcd is generally about <math>O({\rm log}n</math>; square testing can be <math>O({\rm log}n)</math> if you have prime numbers pre-generated, are using <code>sqrt</code> function, or pre-generated all squares and stored in a hash.  Besides these, there's no extra space requirement.

2. Generating triples with Euclid's method; this involves how you choose <math>u, v</math> values.  This is difficult to categorize.  If you are picking those pairs to only generate primitive triples, creating a pair of coprime numbers are at least <math>O(n{\rm log}n)</math> I think, though once a pair is there, no additional work is needed.  If you also generate non-primitive triples, it may be much slower.  If using loops, there's no extra space required.

3. Generating triples with the parent-child relationship.  Each iteration increases perimiter of current triple by about 2, so recursion will run <math>O({\rm log}n)</math> deep, that's also the space requirement (stack space); each recursion produces 3 triples, so running time is 3 to the power of depth, which is <math>O(n)</math>.  Non-primitives come with no extra cost.  For this particular task, it may be the most efficient method speed-wise. --[[User:Ledrug|Ledrug]] 23:56, 29 June 2011 (UTC)

:Note that "For this particular task, it may be the most efficient method speed-wise." probably assumes a low-parallelism implementation.  If we had an infinite number of cpus, method 2 might be faster, because after treating one potential triple the work of any one cpu is done.  In method 3, a cpu must continue its work, building further triples based on an initial triple.  Obviously, of course, this way of thinking is not relevant in the context of a single cpu with only one core.  So, if method 3 is faster, there would probably be a limit on the number of available cpus to handle computations for a given perimeter.  (This kind of thinking might become relevant in GPU implementations.) --[[User:Rdm|Rdm]] 13:43, 16 December 2011 (UTC)

:: No doubt parallelism can be important, but the approximation of an ''infinite'' number of parallel processing unit is a little too blindly exuberant (10^9 perimeter has 70M primitives, which is too high for number of cores, GPU units or CPUs on a cluster).  Even if we can count on this optimism, there are two points to consider: a) the recursive generation of method 3 can be parallelized.  At each step, there are three branches for next step, and you could dispatch them to separate units.  It fully "fans out" after log<sub>3</sub>N steps; as long as number of cores is not comparable to number of primitives, this is negligible.  b) method 2 needs to scan a range of numbers to pick out coprime pairs, which is a significant overhead, while method 3 always generates triples that satisfy requirements.  Well, the third of the two points is: c) The recursion is very simple, almost certainly simpler than dispatching to multiple cores or GPU units.  Even if you have highly parallel hardware, it might be better to limit how far you want the code to be parallelized.
:: The point c above is why I don't quite buy the J "we can run this code in parallel later" mantra (or the Perl6 "it will hopefully be efficient when it's implemented" notion): parallelism is highly complicated, and though automated optimization may work for some common scenarios, you can't rely on it to work out for everything everywhere.  Sometimes you have to optimize stuff by hand: you sort of know what ''should'' happen, the compilers don't.  If they could figure that out, there would be no halting problem. --[[User:Ledrug|Ledrug]] 02:03, 17 December 2011 (UTC)
::: Well, ultimately, discussions like this are not what counts -- it's implementations where you can measure timing (though also you can sometimes see factor of 2 differences in timings, running the same code on the same machine, and timings themselves need to be taken with a grain of salt).
:::That said... it's quite true that an "infinite number of cpus" is not a practical system -- it's a simplification for algorithm analysis (much like big-O notation is a simplification).  But it's also true that GPUs do not rely on having a physical execution unit for every pixel.
:::Anyways, roughly speaking, on modern CPUs, the biggest time eater is the cache miss.  After that it's the failed branch prediction.  Actual computation is almost (but not quite) free, in comparison.  GPUs shift the balance somewhat (with tighter limits on branching, and more heavily favoring pushing decision making to a higher level in the code).
:::So, for example, I'm not sure what Perl has to do with this kind of analysis.  (Perl, in my experience, tends to have extremely high quantities of branches, in its implementation.)  --[[User:Rdm|Rdm]] 14:44, 19 December 2011 (UTC)

== Ready to graduate? ==

I think this task is pretty stable and clear. It seems like it's ready to come out of draft. Objections? --[[User:Mwn3d|Mwn3d]] 19:53, 4 August 2011 (UTC)

== Strengthening the condition ==
I see that there's a condition that <math>a \leq b \leq c</math>, but I believe that (with the exception of the case where all values are zero) this can be strengthened to <math>a < b < c</math>, because if <math>\mathit{a} = b</math> then it means that either <math>\mathit{c}</math> is zero (in which case they can't be coprime, and aren't positive integers anyway) or its square is exactly twice the square of <math>\mathit{a}</math> (impossible because the square root of 2 is irrational).

Maybe that will help someone. :-) –[[User:Dkf|Donal Fellows]] 10:23, 11 August 2011 (UTC)
: *shrug* I changed it.  Though I doubt it will help anyone: even if one is using brute furce, the right triangle condition provides a much stronger constraint anyway. --[[User:Ledrug|Ledrug]] 21:22, 11 August 2011 (UTC)

== Math tags broken? ==

I see literal <nowiki><math></math></nowiki> tags everywhere...<br/>Is this a bug introduced by the server switch perhaps?
: Looks like it. Already reported to site owner. –[[User:Dkf|Donal Fellows]] 13:58, 14 December 2011 (UTC)
:: Still not working in Chrome? --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 01 May 2016

== Wrong? ==

Both [[wp:Pythagorean_triple|Wikipedia]] and Sloane's [https://oeis.org/A020882 A020882] agree that there are 16 primitive Pythagorean triples with c up to 100:

: (3 , 4 , 5), (5, 12, 13), (7, 24, 25), (8, 15, 17), (9, 40, 41), (11, 60, 61), (12, 35, 37), (13, 84, 85), (16, 63, 65), (20, 21, 29), (28, 45, 53), (33, 56, 65), (36, 77, 85), (39, 80, 89), (48, 55, 73), (65, 72, 97)

Why do all of our solutions give 7 instead of 16? Which nine aren't counted?

[[User:CRGreathouse|CRGreathouse]] 06:36, 13 September 2012 (UTC)

:''The task is to determine how many Pythagorean triples there are with a perimeter no larger than 100 and the number of these that are primitive.''  So, for example, the triangle with side lengths 48, 55, 73 has a perimeter of 176.  --[[User:Rdm|Rdm]] 14:18, 13 September 2012 (UTC)

:: Got it. For some reason I kept reading it as c not P. [[User:CRGreathouse|CRGreathouse]] 00:12, 14 September 2012 (UTC)


==Several formulae rendered invisible to many browsers by cosmetic edits==

Cosmetic edits made to the task page on May 1 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left several of the formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:48, 20 September 2016 (UTC)

: Removed redundantly injected space around &lt;math&gt; tag contents, restoring visibility of server-side formula graphics [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:43, 30 October 2016 (UTC)
