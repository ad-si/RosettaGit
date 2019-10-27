+++
title = "Talk:Range expansion"
description = ""
date = 2015-11-24T02:18:28Z
aliases = []
[extra]
id = 7776
[taxonomies]
categories = []
tags = []
+++

==Negative ranges==
Should the test case contain a negative range, such as -15--10? [[User:Stormneedle|Stormneedle]] 18:57, 16 July 2010 (UTC)

: It was carefully chosen to have a negative number as I found it complicated the Python solution I was working on. Self flagellationarily yours, --[[User:Paddy3118|Paddy3118]] 19:17, 16 July 2010 (UTC)

:: I think he was asking if the test case should be extended to include a range where the terminating number is negative.  For example, perhaps -3--1 instead of -3-1?  --[[User:Rdm|Rdm]] 19:21, 16 July 2010 (UTC)

::: Yes, I was, Rdm. Sorry that I was unclear. [[User:Stormneedle|Stormneedle]] 21:35, 16 July 2010 (UTC)

: Ahh. I understand now. Yes. I'll make the change within the hour, but it will invalidate all current entries. (How do you flag all entries for update due to a change in the task? --[[User:Paddy3118|Paddy3118]] 04:59, 17 July 2010 (UTC)

== Spaces in the list ==

The examples are all without spaces in the list, e.g. "1, 3-6". I guess this means that solutions which don't allow for spaces are correct, right? What about the converse: Would an example which does allow for spaces be incorrect? In other words: Is accepting spaces mandatory, optional or forbidden?

BTW, my C++ code accepts whitespace between numbers and commas/range dashes as well as at the beginning and end (the latter is especially handy for reading directly from standard input, because with normal usage there's always a line feed at the end) --[[User:Ce|Ce]] 09:12, 17 July 2010 (UTC)

: For the purposes of examples given to tasks, no spaces should be given to expansion routines or created from extraction routines. There is however, no need to ''ensure'' that you can only work within such restrictions however. --[[User:Paddy3118|Paddy3118]] 10:11, 17 July 2010 (UTC)

:: Thanks for the clarification. --[[User:Ce|Ce]] 11:14, 17 July 2010 (UTC)

: Both versions of the REXX examples also "accept" (ignore) blanks, but doesn't use them as delimiters.  I see nothing wrong with whitespace if it helps readability (for human eyes). It's easy enough (programatically) to remove superfluous blanks. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:29, 12 August 2012 (UTC)

== Result format, Unary + ==
Is the result supposed to be a string of comma-separated integers, as in the example? Or a legal list literal in the language? What if the two are different? I added the Perl, where the two happen to be (almost) the same. But that's not going to be the case for every language. --[[User:Snoman|Snoman]] 22:08, 17 July 2010 (UTC)

Also, what about handling unary + on positive integers in the input string? Or just assume that positive integers are always unmarked? I guess this could also be language-dependent. --[[User:Snoman|Snoman]] 02:17, 18 July 2010 (UTC)

:Assume there will be no plus characters in the range format; and that the list format is the easiest printed representation of your languages internal list of integers data structure that also reads nicely to humans. (Drawing the pointers of a linked list would be over the top, even if you have the routine to do that handy :-) 
--[[User:Paddy3118|Paddy3118]] 05:44, 18 July 2010 (UTC)

::OK, that's clear, many thanks! Of course, Snobol4 (my other contribution) has no list or array literals - yes, it's an annoyance :-) - so I just followed your example. --[[User:Snoman|Snoman]] 08:02, 18 July 2010 (UTC)

== Ranges oddness ==

What is the "desired" behaviour if the range input is e.g. "4,10-6" instead of "4,6-10"? My current implementation of [[Range extraction]] from "4,3,2,1" produces "4-1", while the current impl of Range expansion won't expand "4-1" to "4,3,2,1"... I have a ready solution where range limits are "ordered", but this way "4-1" is equivalent to "1-4" and produces "1,2,3,4" which is not the original "4,3,2,1"... --[[User:ShinTakezou|ShinTakezou]] 17:36, 18 July 2010 (UTC)

:The task assumes that it gets correctly formatted ranges that expand to an ''increasing'' series of different integers. I know that this most likely would not be the case 'in the real world', but we just need to have comparable examples, and it is convenient to miss out input validation steps.--[[User:Paddy3118|Paddy3118]] 18:08, 18 July 2010 (UTC)

== Alpha Numeric Ranges ==
Would it be possible with the Alpha numeric range expansions also?
Like A-D results A,B,C,D
A1-F1 results A1, B1, C1, D1, E1, F1
A1-A5 Results A1, A2, A3, A4, A5
:Yes, of course, but there are several conflicting implementations possible.  For example, how many elements are in 00A1-00B3?  How about in DOG-MUTT?  How about in 1e6-2E20?  And... so on...--[[User:Rdm|Rdm]] 14:39, 15 November 2011 (UTC)

:And of course, what about ABC-Aef (where upper and lower case letters have different ordering depending upon the underlying hardware [ASCII vs. EBCDIC]). And whose alphabet do we use? Any why would ''that'' particular alphabet be used? Once you start using such things (words), how does one handle a word with a dash in it? Jack-in-the-box springs to mind. (A two-fer). -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:42, 12 August 2012 (UTC)

==Comment from TXR example==
Just to note that although the TXR parser may allow for null ranges, the task description language does not. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:25, 14 January 2014 (UTC)

==Undetected intruder in the example range ?==

I notice that the example given includes a zero in 5th position:

 -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
 Is accurately expressed by the range expression:
 -6,-3-1,3-5,7-11,14,15,17-20

but there seems to be no obvious source/representation of that zero in the range expression, 
and the outputs of the first few code examples that I have looked at duly generate no zero … 

So perhaps a typo ?  Or have I missed something in the discussions above ?

[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:03, 24 November 2015 (UTC)

: 0 is part of "-3-1" --[[User:Jnd|Jnd]] ([[User talk:Jnd|talk]]) 02:11, 24 November 2015 (UTC)

Got it – so the difference between the requested expansion (-3--1) and the example (-3-1), is presumably intentional ?
 
[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:15, 24 November 2015 (UTC)

: Yep, I think the Note has been there for years --[[User:Jnd|Jnd]] ([[User talk:Jnd|talk]]) 02:18, 24 November 2015 (UTC)
