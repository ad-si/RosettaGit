+++
title = "Talk:Substring"
description = ""
date = 2013-10-05T23:15:14Z
aliases = []
[extra]
id = 4692
[taxonomies]
categories = []
tags = []
+++

The individual subtasks here seem to cover only certain particular arbitrary use cases and not others. Why not have
* substring that starts at index n and ends at index m
* substring that starts at index n and ends at m places before the end of the string
* substring that starts at n places before the end of the string and is of length m
* and so on

Also, the last two subtasks seem very obscure and contrived. No language seems to have built-in methods for them. It seems that all the solutions are basically (1) find the character or substring we are looking for, and (2) use the first subtask ("starting from n characters in and of m length") to get the result. Why not just put the finding the character or substring part as a separate article? --[[Special:Contributions/76.173.203.32|76.173.203.32]] 09:28, 10 August 2009 (UTC)

----

> The individual subtasks here seem to cover only certain particular arbitrary use cases and not others.

I thought it would be overly repetitious and verbose to cover all cases.

> Also, the last two subtasks seem very obscure and contrived. No language seems to have built-in methods for them. 

Yes, I think you're right. I expected Ruby to have this feature but it turned out not to. Leaving the only language I know that does as XSLT http://www.zvon.org/xxl/XSLTreference/Output/function_substring-after.html Hardly significant enough to justify those two subtasks. If you're happy to make those changes I'll support them.

[[User:Oligomous|Oligomous]] 17:48, 10 August 2009 (UTC)

----

For what it's worth, the last Snobol4 subtask was incorrect, though it happened to return the right result. The break( ) pattern creates a character class like regex [ ], not a substring to match. Fixed. --[[User:Snoman|Snoman]] 11:32, 12 July 2010 (UTC)

In the same way that we have "whole string minus last character", we also need "whole string minus first character here", because there may be a separate handler within the language for removing a single leading character without needing to substring from characters 2 to end.

[[User:Markhobley|Markhobley]] 23:30, 2 June 2011 (UTC)

==Substantial task changes affecting many examples==
The more examples their are for a task, the more effort it takes to change the essential task goals '''and get all the examples updated'''. This task is not draft and has 60 examples. You need to weigh any change to the task definition against the ability to get most of the examples updated, and I think 60 examples is too much for a change that adds another requirement to the task description when the task description without it wasn't so bad.

What do others think? --[[User:Paddy3118|Paddy3118]] 04:35, 5 June 2011 (UTC)
:I agree. Way too far in to a task effort to make changes without discussion. Also, I'm not sure how many occasions there are to show a string minus the first character. In any case, we should talk about it first. --[[User:Mwn3d|Mwn3d]] 05:03, 5 June 2011 (UTC)

:: (regarding showing a string minus the 1<sup>st</sup> character):   this is covered by the 1<sup>st</sup> task requirement.   ''Showing'' that result isn't very common, but ''using'' a string starting with the 2<sup>nd</sup> is. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:52, 14 March 2013 (UTC)

==special cases==
I am wondering how many languages allow a zero length   '''SUBSTR'''   (or equivalent BIF).
 

Also, the 3<sup>rd</sup> task requirement:   if the (original) string is a null string, how many language examples would handle that case? 

I've been bit in the hinder too many times on that little ditty.     "But, but, but, it never should've happened ..." 

-- [[User:Gerard Schildberger|Gerard Schildberger]] 19:03, 14 March 2013 (UTC)

-----

Also, considering the PL/I version when the original string has a length of zero (a null string, if you will).   What does the PL/I '''substr''' BIF do with a negative length (3<sup>rd</sup> argument)? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:15, 5 October 2013 (UTC)
