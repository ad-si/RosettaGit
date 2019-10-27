+++
title = "Talk:Combinations"
description = ""
date = 2019-07-31T12:31:54Z
aliases = []
[extra]
id = 13617
[taxonomies]
categories = []
tags = []
+++

== Combinations with repetitions ==
The formula for combinations with repetitions is wrong. Can some body correct it with the right formula from http://en.wikipedia.org/wiki/Multiset_coefficient#Counting_multisets.

--[[User:Raghu|Raghu]] ([[User talk:Raghu|talk]]) 13:51, 23 May 2013 (UTC)

: We can't talk about correctness of the equation without also talking about the definitions of the terms which appear in those equations.  A few quick tests show that both the equation on the page for combinations and some of the wikipedia expressions are using different definitions from what would be appropriate for the "3" and "5" used in the initial example for this task.  For example, if we look at all the combinations of five things drawn from a set of 5, we would expect only one valid combination (all of them). If we believe that 5 is n and 5 is k, we come up with 11 factorial divided by 5 factorial times 6 factorial (which is a number different from 1).  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:19, 23 May 2013 (UTC)

== Dynamic programming for enumeration problems ==

The gain of dynamic programming for a problem asking to list all solution rather than simply count them is much less than what is suggested by claims such as "The first solution is inefficient because it repeatedly calculates the same subproblem in different branches of recursion." (Haskell) for instance. The asymptotic complexity is the same, O(number of solutions * size of solution). [[User:Lyxia|Lyxia]] ([[User talk:Lyxia|talk]]) 01:30, 30 April 2017 (UTC)

== Formula ==

Strictlly speaking, combinations with repetiotions 5 of 5 give you 126 variants, just combinations give you just 1 variant
Formula for comb. with repet: http://hijos.ru/wp-content/ql-cache/quicklatex-7f6e17e6e05e9a62c1188bd95ff7e8ad.gif

Also, we should distinguish third thing - when we have a deal with combinations with repetitions with additional condition.
Sorry for this sentence looking like folk verses.  

With respect to you, Ivan Gavryushin (dcc0@).

== PARI/GP ==

The function definition is:<br />
c(n,k,r,d)

c = combinations<br />
n = number of objects<br />
k = sample size<br />
r = ?<br />
d = ?

What are r and d? [[User:Chuck Coker|Chuck Coker]] ([[User talk:Chuck Coker|talk]]) 02:00, 31 July 2019 (UTC)

:You may not be familiar with recursive patterns. The function c is recursive. d is a count of the current depth, r is sort of an accumulator keeping the permutation thus far. So when d=k the combination is complete and r is printed.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:16, 31 July 2019 (UTC)

:: Thanks for the info. [[User:Chuck Coker|Chuck Coker]] ([[User talk:Chuck Coker|talk]]) 12:30, 31 July 2019 (UTC)
