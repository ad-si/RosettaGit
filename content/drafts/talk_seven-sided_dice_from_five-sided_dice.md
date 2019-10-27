+++
title = "Talk:Seven-sided dice from five-sided dice"
description = ""
date = 2018-12-28T23:31:30Z
aliases = []
[extra]
id = 4684
[taxonomies]
categories = []
tags = []
+++

== dead link ==
The Stack Overflow link in the task description is dead --Thu Apr 10 20:57:05 PDT 2014
== numbers on a die ==
It's more common for computer random number generators to generate a random number from 0 to n-1, than from 1 to n. So I propose changing the definitions of dice5() and dice7() to generate integers from 0..4 and 0..6, respectively. It will make the math a little simpler. --[[Special:Contributions/96.238.211.175|96.238.211.175]] 08:26, 9 August 2009 (UTC)
:Hi, please don't change this as it is more common for dice to count from 1. It is better to make the program adapt to the problem in this case. --[[User:Paddy3118|Paddy3118]] 08:56, 9 August 2009 (UTC)

::As noted in the Tcl explanatory text, this is explicitly about making a primitive D5 and creating a D7 from it. (That's also why I use the terms D5 and D7; what programmer hasn't played at least ''some'' D&D? :-)) In any case, no conventional die (the correct singular form of “dice”) numbers from 0. —[[User:Dkf|Donal Fellows]] 10:04, 9 August 2009 (UTC)
::I'm probably the exception that proves the rule about D&D. (My great time waster was PacMan)!  --[[User:Paddy3118|Paddy3118]] 11:10, 9 August 2009 (UTC)

::: I have quite a collection of dice and none of which have a zero (or blank) on them.         --- Well, all except one set.   They are   ''binary dice''   (and pretty hard to find one in the wild),   and are six sized, with just three sets of a '''one''' and a '''zero'''.   But other than that anomaly, I have no dice with zero (or no) pips.    Note that some binary die have the pips numbered (one through six) in binary, but no zero.      -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:30, 28 December 2018 (UTC)

== J solution seems ugly ==

I'd really like someone knowledgeable to look and make J solution more elegant. This straightforward solution doesn't look very good. [[User:Avmich|Avmich]] 21:03, 13 September 2009 (UTC)

:You could use this <code>dice5</code> and either <code>dice7a</code> or <code>dice7b</code> for the main bit:

```j>dice5=: 
:@:?@$&5
dice7a=: 0 8 -.~ 3 >.@%~ 5 #. [: <:@dice5 2 ,~ */
dice7b=: [: (#~ 7&>:) 3 >.@%~ [: 5&#.&.:<:@dice5 */ , 2: 
```


:Then it's just a question on ensuring that you've got enough rolls. You could use the following explicit:

```j

dice7=: monad define
  res=. 0$0
  while. (*/y) > #res do.
    res=. res, dice7a >. 0.75 * y
  end.
  y $ res
)

```

:...or you could create a tacit equivalent using the <code>^:</code> conjunction.--[[User:Tikkanz|Tikkanz]] 00:15, 14 September 2009 (UTC)

:Here is a more instructive version of the main bit of code:

```j

  dice5=: [: >: ] ?@$ 5:       NB. makes a y shape array of 5s, "rolls" the array and increments.
  rolltwice=: [: dice5 2 ,~ */ NB. rolls dice5 twice for each desired dice7 roll (*/y rows, 2 cols)
  base5to10=: 5 #. <:          NB. decrements and converts rows from base 5 to 10
  keepgood=: #~ 21&>           NB. compress out values not less than 21
  groupsof3=: [: >. >: % 3:    NB. increments, divides by 3 and takes ceiling

  dice7c=: groupsof3@keepgood@base5to10@rolltwice

```
--[[User:Tikkanz|Tikkanz]] 01:20, 14 September 2009 (UTC)
