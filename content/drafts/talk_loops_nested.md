+++
title = "Talk:Loops/Nested"
description = ""
date = 2016-04-05T08:00:42Z
aliases = []
[extra]
id = 11736
[taxonomies]
categories = []
tags = []
+++

==Task Clarification==

When the task description said numbers from   1..20,   did it mean integers?  

If not, the REXX example could also use numbers with decimal parts. 

Some languages (like REXX) can handle both at the same time. 

Also, it wasn't stated explicitly, but I assume the array could be fully populated, but without an element being equal to "20".   -- [[User:Gerard Schildberger|Gerard Schildberger]] 02:54, 15 May 2012 (UTC)
: I'd assume it meant integers; if not, the likelihood of encountering a “20” is too small. (Yes, it could be implemented otherwise, but what would be the point of that?) Another reason for assuming integer is that it says <math>[1\ldots 20]</math> and not <math>[1.0\ldots 20.0]</math> –[[User:Dkf|Donal Fellows]] 10:23, 15 May 2012 (UTC)

:: The point that I was asking for clarification.   If the task was to use integers, then just say so.   Otherwise, it's a matter of assumptions, and programs based on assumptions more often than not, make the wrong assumptions.   In most languages, if the array is declared to be integer, then that's what's stored.   Most examples generated their own arrays, so the programmer knows what to expect, so whatever the programmer assumed is what's stored in that array.    There's no possibility of anything else stored there {except for a possible   ''not defined/assigned''   or   ''integer-out-of-range'',   or some such, if the language supports those kind of things}.   In any case, a program shouldn't blindly assume that there   ''is''   a value of   '''20'''   in the array (no mention in the task was made about the array's size, although it did say it was "filled with...",   and one could make the assumption that it implies more than none).   The number of elements could be zero, five, or five thousand.   I'd rather see a good example than one that made too many assumptions.     9.5   is still between   1 and 20.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:50, 15 May 2012 (UTC)
