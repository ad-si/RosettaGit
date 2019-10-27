+++
title = "Talk:Elementary cellular automaton/Random Number Generator"
description = ""
date = 2019-07-28T20:10:41Z
aliases = []
[extra]
id = 17420
[taxonomies]
categories = []
tags = []
+++

== Endianness or MSB/LSB difference? ==

The description urges "little endian" (byte order in longer integer types), but was really talking about arranging bits in a byte.  Misnomer? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 18:54, 20 March 2014 (UTC)

:It seems so.  My bad.  Is "with the least significant bit at the end" correct?--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 09:55, 21 March 2014 (UTC)

:: Yeah, or "most significant bit first". --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:47, 21 March 2014 (UTC)

==Probably needs more restrictions==
Rule 30 with fixed '0' ends just goes to '1' in all cells after a while. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:33, 21 March 2014 (UTC)

:Have you tried with many different array sizes?--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 09:56, 21 March 2014 (UTC)

::100 (as used in Perl),and also 10. I think it may be down to me using fixed '0' boundaries but this is allowed in the Eca task? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:04, 21 March 2014 (UTC)

:::If you can confirm that with fixed boundary states, all ECA converge towards a finite state of "all 1" for any array size, then indeed it might be better to enforce warping arrays.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:04, 21 March 2014 (UTC)

:::: It took me more than a few moments to discover that   '''eca'''   stood for   <big>''<u>e</u>lementary <u>c</u>elluar <u>a</u>utomation''</big>.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:10, 28 July 2019 (UTC) 

::I can confirm that for 100 and 10 it does go to 1's for column that starts with a 1 in it doing Rule 30. Seems to me that you can't do [[Elementary cellular automaton]] and this task without making the former more specidic? What did you do with boundaries for your solution that works? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:26, 21 March 2014 (UTC)

:::I did wrap them, as I found it much easier to implement.  I will narrow the requirement in the parent task.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 02:24, 22 March 2014 (UTC)

::::Thanks Grondilu :-) 
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:34, 22 March 2014 (UTC)

==C Entry==
Perhaps the C entry is performing some undefined behavour (out-of-bound shifts at run-time, found statically with the D compiler):

```C
for (state = i = 0; i < N; i++)
    if (rule & B(7 & (st>>(i-1) | st<<(N+1-i))))

```

