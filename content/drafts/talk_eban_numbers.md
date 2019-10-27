+++
title = "Talk:Eban numbers"
description = ""
date = 2019-05-26T16:58:50Z
aliases = []
[extra]
id = 22236
[taxonomies]
categories = []
tags = []
+++

== Optimizations? ==
From the task description:

<blockquote>Only numbers less than one sextillion <strong>10<sup>21</sup></strong> will be considered in/for this task.

This will allow optimizations to be used.</blockquote>

I am mildly curious as to what optimizations this refers to. Maybe I am missing something, but I am failing to see how limiting to one sextillion offers any way to optimize. Not so big a deal for the Perl 6 example as it counts the *-ban numbers up to one sextillion 3 times in less than a second (on my system). I just wonder what I am overlooking. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 21:06, 22 March 2019 (UTC)

: See the   '''REXX'''   solution   (I think it has the best comments for eliminating numbers that have an   '''e'''   in them).   At this time, I believe there are four other computer programming language solutions that use (more or less) the same algorithm. 

: The algorithm roughly is:  
:::*   separate the (decimal) number into   ''periods''   (into groups of three digits starting from the right).
:::*   for the   ''units''   position, eliminate one, three, five, seven, eight, and nine.
:::*   for the   ''tens''   position, eliminate ten, eleven, twelve, teens, twenty, seventy, eighty, and ninety.
:::*   for the   ''hundreds''   position, eliminate all numbers greater than zero and aren't a blank, as   ''any''   hundr'''<u>e</u>'''d has an   '''e'''. 


: This algorithm will work up to the sextillions   (and beyond the septillions, it's hit and miss, so to speak).   There are some numbers that have multiple spellings, so I included that sextillion limit to bypass these minefields.       -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:31, 22 March 2019 (UTC)

: Also of note, the algorithm mentioned (above) should have the numbers pluralized, but then   '''twenty'''   would become   '''twenties'''.   and I thought the 2<sup>nd</sup> spelling would maybe confuse some people.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:31, 22 March 2019 (UTC)

::Ah. Makes sense. I went a different way with Perl 6 but I can see your point. Thanks. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:56, 22 March 2019 (UTC)
