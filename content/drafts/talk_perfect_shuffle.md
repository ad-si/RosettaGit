+++
title = "Talk:Perfect shuffle"
description = ""
date = 2015-06-16T13:06:21Z
aliases = []
[extra]
id = 19263
[taxonomies]
categories = []
tags = []
+++

== Hostile task requirement? ==

When I try to post the result specified by this task, I get a message that the website is offline. Perhaps this is because that result is a 21010 character long line of text. But something similar happens when I try to post the result as a table with 53 lines of 253 characters. Perhaps the task should be changed to not require such a large result? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:10, 15 June 2015 (UTC)

:Just elide the result, like the other solutions do :) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 18:55, 15 June 2015 (UTC)

:: I did elide most of it, but why shouldn't this be a part of the task requirements? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:07, 15 June 2015 (UTC)

::: If everybody (hopefully) will be eliding the output (to something reasonable), why have the Rosetta Code task ask for 10,000 shuffles?   Why not just ask for twenty shuffles (up to a deck size of forty)?.   That would make outputs somewhat homogenized.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:15, 15 June 2015 (UTC)

:::: Ok, never mind. I managed to get the elided part out there also. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:16, 15 June 2015 (UTC)

::: Did anybody realistically think that the programming entries (examples) would actually post 5,000 lines of output (I don't see one long line being easy to peruse).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:20, 15 June 2015 (UTC)

:::: "lines"? That's not really the problem. The task does however require 5000 numbers for the result (more if you include labeling - but that is left open). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:03, 15 June 2015 (UTC)

::::: Yes, I meant lines (of "output" to the terminal)--- and the ability to peruse the results in some meaningful way.   I had thought that the printing/including the number of requested results (or lines) is the problem, else why would people elide the results?   If 5,000 numbers are just listed in word-wrap form (or even just one line), how can anybody read such a mass of numbers (or even verify any particular number)?    Adding labels or some sort of index (rank) number would definitely help in the readability, but it would add to the (huge) bulk.   I would hope everyone could see the futility of including (printing) such a list for each entry as part of this Rosetta Code task, other than it being an exercise to see if the  programming entries could create 5,000 numbers.   However, if the task says to print 5,000 results, I'll change my entry to include them.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:36, 15 June 2015 (UTC)

:::::: As I tried to state in my original paragraph - putting all those numbers on a single line crashes the wiki implementation. (Not permanently, but you can't save an edit which has the full result on a single line.)  But ... are you saying that you didn't read that original paragraph nor the task description? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:43, 15 June 2015 (UTC)

::::::: No, I'm not saying (or implying) that I didn't read about the failure, and I have read the Rosetta Code task's requirements.   It says to print (essentially) 5,000 results.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:52, 15 June 2015 (UTC)


###  What to do? 


:Seems like a flaw to me to. Why not edit the task to ask for shuffle counts for 2^(2*n) for n = 1..7 i.e only for counts: [4, 16, 64, 256, 1024, 4096, 16384] 
:Trying for: easy to state, similar size maximum, reduced output.
:Although I haven't run any code to see if the 16384 calculation is reasonable, time-wise.
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:33, 15 June 2015 (UTC)

:: Well... in general the cycle length for this task for a sequence of unique values of length 2^N should be N. So even if you brute force it, that form of the task shouldn't take that long. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:40, 15 June 2015 (UTC)

:::'''Who would object''' to my suggested change and why? I did not start the draft task or do a language implementation but the task as written could be improved by defining drastically shorter output requirements. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:01, 16 June 2015 (UTC)

:::: I cannot think of any grounds for objection to shortening the sequence. Though for a decent workout, perhaps the sequence should be factorial values: 2 6 24 120 720 5040? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:20, 16 June 2015 (UTC)

:: When changing the solutions to only do the calculation for the 7 deck sizes suggested by Paddy3118, the Python solution finishes in 0.11 seconds and the Perl solution in 0.12 seconds, on my machine. With the values suggested by Rdm it takes a bit longer, but still reasonable (1.32 sec and 3.29 sec respectively).
:: So, +1 from me for this change (with either one of those sets). I'd also suggest listing the expected inputs and outputs as a "Test Cases" table in the task description, like I tend to do [[Convert_seconds_to_compound_duration|in my tasks]]:
::{| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles)''
|-
| 4 || 2
|-
| 16 || 4
|-
| 64 || 6
|-
| 256 || 8
|-
| 1024 || 10
|-
| 4096 || 12
|-
| 16384 || 14
|}
:: Or:
::{| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles)''
|-
| 2 || 1
|-
| 6 || 4
|-
| 24 || 11
|-
| 120 || 24
|-
| 720 || 359
|-
| 5040 || 2519
|}
:: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 07:23, 16 June 2015 (UTC)

:: Or how about these inputs, for a nice mix of interesting cases:
::{| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles)''
|-
| 2 || 1
|-
| 100 || 30
|-
| 720 || 359
|-
| 1020 || 1018
|-
| 1024 || 10
|-
| 10000 || 300
|-
| 65536 || 16
|}
:: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 07:52, 16 June 2015 (UTC)

::: +1 on using your third and last set of numbers (and the table). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:33, 16 June 2015 (UTC)

:::: Some other possible sequences include 2^(2 .. 15) - 2:
::::{| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles)''
|-
| 2 || 1
|-
| 6 || 4
|-
| 14 || 12
|-
| 30 || 28
|-
| 62 || 60
|-
| 126 || 100
|-
| 254 || 110
|-
| 510 || 508
|-
| 1022 || 340
|-
| 2046 || 204
|-
| 4094 || 4092
|-
| 8190 || 774
|-
| 16382 || 16380
|}

:::: And 3^(2 .. 14) -3:
::::{| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles)''
|-
| 24 || 11
|-
| 78 || 30
|-
| 240 || 119
|-
| 726 || 140
|-
| 2184 || 1044
|-
| 6558 || 3198
|-
| 19680 || 2980
|-
| 59046 || 168
|-
| 177144 || 43332
|-
| 531438 || 6776
|-
| 1594320 || 397380
|}

:::: That last value might be excessive, but looking at the original task, for a sequence of 9950 numbers I get a cycle length of 9948. So if we are being true to the original task description I imagine we should include something similar in the updated requirements? Or is the 9950 example already an unreasonable burden? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:44, 16 June 2015 (UTC)

::::: I think 1020->1018 is sufficient as a "high number" example. There's nothing really to be gained by going higher, except for performance bench-marking but that's not what rosettacode is supposed to be about. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 13:04, 16 June 2015 (UTC)

: I felt like doing some improved HTML diagrams, so I went and updated the whole task while I was at it, using a list of values similar as I suggested above, but amended slightly to include 52 (the size of a standard card deck) and 8 (the example given in the introduction). It also has two powers of 10, two powers of 2, one factorial, and one number where the result is almost as big as the deck size (and also pretty right in absolute terms). I think that should cover everything of interest. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 13:01, 16 June 2015 (UTC)
