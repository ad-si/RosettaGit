+++
title = "Talk:Determinant and permanent"
description = ""
date = 2018-11-13T14:37:13Z
aliases = []
[extra]
id = 12098
[taxonomies]
categories = []
tags = []
+++

==Method used==
Should the algorithm be as stated in the equations? i.e. using the sign function? --[[User:Paddy3118|Paddy3118]] 07:17, 24 July 2012 (UTC)

: Any method is acceptable, use the one that best fits the language and your preferences. (If you like you can write multiple interpretations and compare them: one by the definition, one terse, one fast, one space-efficient, etc.)
: In general I don't think that tasks should over-constrain authors. The purpose of the site is to compare different languages, and we can't effectively do that if we force everyone to write in the same style. "Real Programmers can write FORTRAN in any language", they say...
: [[User:CRGreathouse|CRGreathouse]] 17:30, 27 July 2012 (UTC)

::Hi CRGreathouse, it wasn't so much the style that I wanted clarification on, but the algorithm to be implemented. On RC there are a whole suite of sort tasks - they all do the same thing, just with different algorithms. Similarly, when made aware of the signed permutation method of calculating a determinant via this task and its links, (my thanks), I thought there maybe room for something similar? --[[User:Paddy3118|Paddy3118]] 06:14, 28 July 2012 (UTC)

::: I think some of these "differences of algorithm" would go away if we thought of multiplication as being a short circuit operator (that could prune entire recursive call trees when one of the values being multiplied was zero).  Another thing that might help would be re-expressing the algorithm using something other than a "sequence over time loop" to organize the recursive expansion of minors (or at least: allowing rephrasings which ignore the limitations of ieee floating point.    That said, a little focus on re-expressing using equivalencies (and searching for likely zeros) does seem like a good subject when dealing with alternative algorithms..  --[[User:Rdm|Rdm]] 13:29, 30 July 2012 (UTC)

== Why "arithmetic"? ==


I don't get what this task has to do with arithmetic.  Isn't it about invariants of a matrix, or something like that?--[[User:Grondilu|Grondilu]] 16:38, 5 January 2013 (UTC)

== FORTRAN parallel effort ==

Please credit additional contributors from 
[http://forums.devshed.com/other-programming-languages-139/fortran-parallel-945576.html#post2878816]
--LambertDW 20:45, 21 May 2013 (UTC)
