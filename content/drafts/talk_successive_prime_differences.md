+++
title = "Talk:Successive prime differences"
description = ""
date = 2019-04-28T15:43:27Z
aliases = []
[extra]
id = 22296
[taxonomies]
categories = []
tags = []
+++

==References==
Could you please add a title to the references as well as the link. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:43, 28 April 2019 (UTC)

== Different results for 6,4,2 ==

I am getting slightly different results for the last group than the Python example and am finding it hard to see where I may be wrong. I am finding 337 groups. Am I just mising something? --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 01:20, 27 April 2019 (UTC)

Doh! It's me, I glossed over the 'successive' part of the problem. Update to follow. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 01:25, 27 April 2019 (UTC)

==Task background==
It's my birthday soon so I googled my age and found:
* It's a prime.
* It's a twin prime.

I searched Rosetta Code and found that there was no twin prime task! (I had expected that someone would have already started it). I resolved to wait until closer to my birthday then put up a twin primes task and left it at that.

A few days later I started to think of what a generalisation around the idea of twin primes would be and hit on a difference; then multiple differences; then really liked how my solution to generating a sliding group of <nowiki><count></nowiki> items from a list actually did come from the Python fundamentals:
:
```python
zip(*(lst[n:] for n in range(count)))
```


I finished the code and played with the differences then firmed up what the task details would become. I wrote the task and added extra explanations and emphasis to try and help the reader grasp the details, then went to bed.

Today I've just done a search of the primes generated from differences of <code>2, 4</code> on OEIS to find that it is [https://oeis.org/A275515 known] to some degree, but expressed differently and not as generally as here - I guess recreational maths peeps think alike :-)

Enjoy. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:54, 27 April 2019 (UTC)
:It is very well studied, but you must state it slightly differently. Let P2 be the infinite sequence of successive primes (p2_a,P2_b) such that P2_b-P2_a=2. and P4 be the similar infinite sequence (P4_a,P4_b) such that P4_b-P4_a = 4. The your generalization to P2P4 as 3 successive primes with Pa,Pb,Pc with Pb-Pa=2 and Pc-Pb=4 is a search through P2 and P4 to find P2_b=P4_a. An interesting study would be to compute over a large range the length of P2 and P4 and thus predict the length of P2P4. For a given range should the length of P2P4 be the same as P4P2?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:28, 27 April 2019 (UTC)

:: I was going to add a twin prime task   (and cousin prime task, a difference of four),   but was somewhat preempted with addition of the   ''sexy prime''   task   (a difference of six),   so I dithered a bit.   There are other named difference primes such as   ''devil''   (also called   ''beast''),   ''centennial'',   and   ''millennial''   primes.   However, having a Rosetta Code task just for twin primes would make the code a   lot   cleaner and simpler,   not to mention faster.   This would've made the task solutions more easier to compare   (and I think more useful for people who wanted a clean and robust code for just concerning the generation of twin primes).   Plus it would be easier to find when people are looking for a simple twin prime generator.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:25, 27 April 2019 (UTC)

:: I googled my age, and found it to be   ''highly totient''.     And I'll never be   ''highly totient''.   again.   Sigh.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:38, 27 April 2019 (UTC)
::: I enjoyed the google to work that out :-) 
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:06, 28 April 2019 (UTC)
