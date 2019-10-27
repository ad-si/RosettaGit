+++
title = "Talk:Sequence: smallest number greater than previous term with exactly n divisors"
description = ""
date = 2019-04-13T13:43:01Z
aliases = []
[extra]
id = 22262
[taxonomies]
categories = []
tags = []
+++

{{Alertbox|pink|For future readers: This task was renamed from "Anti-Primes Plus" and split into multiple tasks. Any reference to anti-primes plus is a remnant from before the rename.}} 
== OEIS A069654 ==
the first 25 are:
1, 2, 4, 6, 16, 18, 64, 66, 100, 112, 1024, 1035, 4096, 4288, 4624, 4632, 65536, 65572, 262144, 262192, 263169, 269312, 4194304, 4194306 <BR>
6765201 <- <BR>
But using the C-Version with MAX set to 28 the result is:<BR>
The first 28 anti-primes plus are:<BR>
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624 4632 65536 65572 262144 262192 263169 269312 4194304 4194306 4477456 4493312 4498641 4498752 
completly different.
[https://www.nayuki.io/page/calculate-divisors-javascript] 4477456 = 2^4 × 23^4 got 25 divisors like 6765201 =3^4*17^4

== Which integer sequence is meant output for F# ==
I don't understand the output of the   '''F#'''   entry.   Where is the list of the first '''15''' numbers of the ''anti-prime plus'' sequence?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:59, 9 April 2019 (UTC)

:It is contained within the list of the first 58 terms directly after the task entry. You may need to scroll down a bit to see it. HTH --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:48, 9 April 2019 (UTC)

:: I can scroll just fine.   OK, I see the first five numbers in the sequence just fine.   The sixth number shouldn't be   '''12''',   it should be   '''18'''.   And, where's the tenth entry that should be   '''112'''?   There isn't a   '''112'''   anywhere to be seen in the output.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:13, 10 April 2019 (UTC)

:: Never mind, I see that   '''F#'''   is computing a "different" sequence.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:16, 10 April 2019 (UTC)

::: For what it's worth, the term "The Anti-primes plus sequence" doesn't seem to exist anywhere on the web except here, so I wouldn't get to hung up on what is '''the''' correct sequence. The task description is extremely vague. Most people seemed to interpret it as [[oeis:A069654|OEIS: A069654]], but [[oeis:A005179|OEIS: A005179]] is also a valid sequence that fits the description. I also included two other "sequences" in the Perl 6 entry that technically satisfy the requirements. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 00:30, 10 April 2019 (UTC)

:::: Since this is still a   ''draft task'',   do you think that the task definition/requirement should be tightened up   (and/or refined as to make it   ''un-vague'')   so that all computer programming solutions/entries are solving the same task?   The main purpose of Rosetta Code   (I think)   is to compare programs, but if some programs are solving a different requirement than the others, it's impossible to compare algorithms.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:33, 10 April 2019 (UTC)
::::: Let creator of the task decide or the first entry, which was [[Anti-primes_Plus#Go|GO]] which leads to [[oeis:A069654|OEIS: A069654]] -- [[User:Horsth|Horsth]]

:::::: I didn't have anybody else in mind, the task's author is almost always the best person to do the tidying up.   But from my experience, it seems that everybody has an opinion of what's a better way to phrase things, and I can talk from days of past that task requirements and phrasing have been changed/modified/mucked way beyond what the task author intended.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:25, 10 April 2019 (UTC)

::::::: Really? Why would someone who understands number theory not be better?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:20, 10 April 2019 (UTC)
:::::::: Perhaps we should have two tasks: A005179 Smallest number with exactly n divisors; and A069654 a(1) = 1; for n > 1, a(n) = smallest number > a(n-1) having exactly n divisors. 
 If we can only have one I would favour A005179 for its number theoretic interest. Why does the sequence have spikes at prime n? Anyone proposing this task should be able to answer this question!!!! --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:20, 10 April 2019 (UTC)
::::::::: Already said in [[Anti-primes_Plus#Pascal|Pascal]]. waitung for CalmoSoft :-) -- [[User:Horsth|Horsth]]
:::::::::: I'm sure that why the sequence has spikes at prime n hasn't been adequately said! I've now had my say at http://www.rosettacode.org/wiki/Talk:Sequence:_smallest_number_with_exactly_n_divisors.

== Rename and split ==

This task is worthwhile but extremely poorly named and specified. The task has almost nothing to do with anti-primes other than it uses divisors. There is no other use of the term "anti-primes plus" that Google, Bing or Duckduckgo can find anywhere on the web so the name is suspect at best, and it doesn't really capture what the task is supposed to do. [[User:Calmosoft]] (the original author) doesn't speak English as his first language so demanding that he come up with a better name / description may be futile.

I propose to rename this task and add two other tasks. Since most people (including the original author) seemed to interpret this task as [[oeis:A069654|OEIS:A069654]], this task will become that. (smallest number of examples that will need to be moved/modified)

:*(this task) Sequence: smallest number greater than previous term with exactly n divisors ([[oeis:A069654|OEIS:A069654]]) 
:*(new task) Sequence: smallest number with exactly n divisors ([[oeis:A005179|OEIS:A005179]])
:*(new task) Sequence: nth number with exactly n divisors ([[oeis:A073916|OEIS:A073916]])

if you have a suggestion for a better/shorter name for any of these that is descriptive enough to be able to tell essentially what the task is asking for, please share it. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 15:16, 11 April 2019 (UTC)
