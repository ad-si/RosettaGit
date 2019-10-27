+++
title = "Talk:Two Sum"
description = ""
date = 2017-12-05T17:47:50Z
aliases = []
[extra]
id = 21144
[taxonomies]
categories = []
tags = []
+++

== Some ambiguities (inappropriate return type)==
What if there is more then one way to get the desired sum? Should it return '''all''' of the pairs or only '''a''' pair? Are we to assume the integers in the array are unique? --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 22:21, 4 October 2016 (UTC)

What if there is no solution ?
E.g. when all numbers in the list are even, 
and the desired sum is odd.  -- [[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 13:53, 5 October 2016 (UTC)

One way of putting it is that the proposed return type (list of integers '''[Int]''') is not quite right yet. The structure of the problem would be more clearly expressed by requiring the return of a list of pairs of integers '''
```txt
[(Int, Int)]
```
'''
i.e. Returning an empty list where no solutions are found, and a list of more than one integer pair where multiple solutions are found.

The English formulation of the task may also need a slight tweak – the phrase "If so, return indices of '''the''' two integers" skips a bit heavily over the thin ice – it seems to express an assumption that any solution would necessarily be unique. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 07:55, 17 October 2016 (UTC)

== (from the task description) Given a sorted array of positive integers ... ==
Given that the example list is an ordered list (of positive integers),   then why is   ''zero''   included in the array?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:17, 19 January 2017 (UTC)

Will either the mention of   ''positive integers''   be amended, or will the list of given numbers be changed   (hopefully before the draft task gets promoted)? 

Since everybody has already used the example list   (that contains a non-positive integer, namely zero),   it would probably be easier to change it to: 

  '' ... Given a sorted array of non-negative integers ...''

  -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:17, 23 September 2017 (UTC)
:I updated the task. The assumption that the integers are positive (or non-negative) is absolutely not necessary. Also, that integers are "single" (unique?) does not guarantee a unique solution, so it's useless. For instance, look for the sum 9 in (1,2,7,8). And to see negative integers don't hurt, just add 1-min(a) to the array 'a', and add twice this number to the target. Actually, one could do the same with non-integers, but it would introduce unnecessary difficulties (floating-point is not exact, hence one can't rely on equality comparison). [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:15, 4 December 2017 (UTC)

:: You're preaching to the choir.   However, my objection was that the   ''given''   of non-negative integers as part of the specification, but zero was included in the case example, a confliction.   Now that negative integers are allowed, some programming examples will need to be changed.   Also, (for the 2<sup>nd</sup> REXX programming example),   I've added a test case that included negative numbers as well as a duplicate number.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:01, 4 December 2017 (UTC)

:: Also, as for the use of non-integers, that isn't a problem for some languages, as not all floating point numbers are necessarily stored in binary, some programming languages can use   ''decimal''   floating point.   Other programming languages can also support scaled numbers, that is,   '''2.3'''   can be stored as an decimal integer, with the decimal point indicated to the appropriate location.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:01, 4 December 2017 (UTC)
== 11 + 11 = 22 ==
In the given example do 11 + 11 constitute a pair of integers? Is [0,2,11,11,90] a valid sorted array?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:00, 5 December 2017 (UTC)
:I updated the task to explicitly allow this. It's not a problem, at least with the O(n) algorithm given in the SO source. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 17:47, 5 December 2017 (UTC)
