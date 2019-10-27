+++
title = "Talk:Closures/Value capture"
description = ""
date = 2012-12-20T21:32:26Z
aliases = []
[extra]
id = 10126
[taxonomies]
categories = []
tags = []
+++

== Sample output ==
Sample output is always useful in the task description.  
While the task says display, the requirement is there but what is left up to the implementer. 
The Python example does not show it's output.
--[[User:Dgamey|Dgamey]] 11:33, 20 July 2011 (UTC)

:The Python expected output seems to be in comments after the prin statements. --[[User:Paddy3118|Paddy3118]] 12:39, 20 July 2011 (UTC)

==Function description a bit vague==
A  bit more specification/clarity would be helpful.  I think I understand, but I shouldn't have to read one of the code examples to be sure.
:The description "such that the function at index i (i from 0 to 9), when run, should return the number i." sounds like the function just returns its argument.  It says nothing about how the return value behaves.  It looks like we are meant to use a closure to create what is effectively a constant.
::I misinterpreted this as its parametric position at first, but I think it means its position within a list. What happens if the list is as follows?: a(), b(), a(),
::Should the first a() give 1, b() give 2, and the second a() give 3? [[User:Markhobley|Markhobley]] 12:06, 20 July 2011 (UTC)
::: It seems to create a list of 10 elements with each element being a call to the same function bound to the index position.  Which is why it struck me that it is using a closure to simulate a constant.  (Mind you shuffling the list elements could then be fun).  --[[User:Dgamey|Dgamey]] 13:09, 20 July 2011 (UTC)
:::: It says a list of 10 functions. It must be permissible to use different functions, albeit, some of them may be listed more than once. If there was only one function repeated 10 times, it would not be much of a list (a bit like a shopping list for beer, beer, beer and beer): We might as well just pass the function just once, do away with the list, and just utilize the function ten times (or pass the function as the first argument and the number of utilizations as the second). [[User:Markhobley|Markhobley]] 17:16, 20 July 2011 (UTC)

: Why 10 functions? Surely two or three would be sufficient to demonstrate how this works?  The python example seems to put these in a list.
--[[User:Dgamey|Dgamey]] 11:46, 20 July 2011 (UTC)

==Proposed task tweak==
Wouldn't it be better if this task were to have functions/closures that didn't just return the value? Right now, the value returned is the same as the index into the list, which makes it harder to interpret the output. (Using the square of the value would be just as easy to implement, and yet be far clearer.) –[[User:Dkf|Donal Fellows]] 08:27, 21 July 2011 (UTC)
: I would be agreeable Unicon contributor --[[User:Dgamey|Dgamey]] 10:04, 21 July 2011 (UTC)
: Okay by me too. --[[User:TimToady|TimToady]] 20:48, 21 July 2011 (UTC)
:: I went ahead and changed it.  Also changed the index requirement to 1-based, because to me that feels more language-neutral somehow.  --[[User:Ledrug|Ledrug]] 01:19, 22 July 2011 (UTC)
:::If you want it language neutral, then don't mandate either 0 or 1 based indexing.  It doesn't really matter much for demonstrating the feature.  What does seem to matter a bit more is that the square be the square of the index, whichever is chosen, and we now have solutions where f[7] == 64, which bugs me somehow. --[[User:TimToady|TimToady]] 14:39, 22 July 2011 (UTC)
::::Fair enough. --[[User:Ledrug|Ledrug]] 14:52, 22 July 2011 (UTC)
What happens if the list is as follows?: a(), b(), a(),
::Should the first a() give 1, b() give 4, and the second a() give 9? [[User:Markhobley|Markhobley]] 13:42, 22 July 2011 (UTC)

Would this read better as "Create a list of up to 10 simple functions (anonymous functions are encouraged), so that the function returns the square of its position within the list."? [[User:Markhobley|Markhobley]] 13:52, 22 July 2011 (UTC)
