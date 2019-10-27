+++
title = "Talk:Count in factors"
description = ""
date = 2016-07-04T19:36:35Z
aliases = []
[extra]
id = 9013
[taxonomies]
categories = []
tags = []
+++

==task note==
It'd be best to include the "factor" function, and note where it came from. --[[User:Short Circuit|Michael Mol]] 21:23, 23 December 2010 (UTC)

== Draft/Non-draft ==

While I don't see any problems, or have any complaints with peoples' implementations, I want to hold off until Jan 1st before un-drafting; there are a few languages and participants I usually see, but I don't see their solutions yet. --[[User:Short Circuit|Michael Mol]] 18:17, 24 December 2010 (UTC)
: What can I say? Christmas is when good food and drink take precedence over coding… –[[User:Dkf|Donal Fellows]] 21:00, 24 December 2010 (UTC)
:: Hey, I'm not complaining! Enjoy the holidays! :) --[[User:Short Circuit|Michael Mol]] 21:20, 24 December 2010 (UTC)

Perhaps rather than having a special rule for 1, the count should start from 2?  --[[User:Rdm|Rdm]] 19:16, 27 December 2010 (UTC)
: It'd make it neater, algorithmically, but then it defies the simplistic concept of counting. I could rationalize, too, that having the troublesome case of <math>1</math>-is-not-prime, is normal for the problem at hand, and helps expose workarounds and idiomatic approaches for special cases. Really, though, it comes down to the fact that when I count to ten, I start at one. --[[User:Short Circuit|Michael Mol]] 20:02, 27 December 2010 (UTC)
:: Ok, well.. technically speaking, the list of prime factors for 1 is the empty list.  But I suppose representing that might look odd to some people.  --[[User:Rdm|Rdm]] 23:58, 27 December 2010 (UTC)

== duplicate task? ==

What's different to just calling [[Prime decomposition]] in a loop? Is this really worth a separate task? --[[User:Oenone|Oenone]] 09:09, 19 April 2011 (UTC)
: It's not a duplicate, because it's an extension of the behavior of another task, and serves its own purpose (that of showing the factors of a sequence of numbers) --[[User:Short Circuit|Michael Mol]] 12:21, 19 April 2011 (UTC)

==phantom categories - incorrect use of Library templates==
I found this looking into category cleanup.  Library templates (D, Ruby) for modules by name Prime, UIprime create categories.  I would expect the library template would address the name of a general library and then reference a specific member.  This usage is creating clutter all over RC.  

: Knowledgeable Ruby and D users - help please.

--[[User:Dgamey|Dgamey]] 11:40, 18 May 2011 (UTC)

: (With Ruby) 'prime' is a library, and its members are Prime, Prime#prime_division, Integer#prime_division, Prime::Generator23 and so on. The 'prime' library is part of the standard library. I am not wanting phantom categories for libraries of the standard library ('prime', 'optparse', 'strscan', 'find', 'securerandom' and so on), so I am removing them. --[[User:Kernigh|Kernigh]] 16:45, 22 August 2011 (UTC)

==stating that 1 is prime==

I marked Python as ''partly incorrect'' (which was later rescinded) that Python marked '''1''' as a prime, not that '''1''' was included in the listing (with '''1''' as a factor).    It was the ''marking'' of '''1''' as a prime that was indicated as (partly) incorrect.   Other than that, the factors of the integers listed were correct.   Nowhere did I indicate that '''1''' shouldn't be in the list.   I don't know any other method of flagging an entry to address this situation of ancillary output being incorrect. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:56, 27 October 2013 (UTC)

:I've changed it. 1 is "not composite" but that would mess with the formatting. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:07, 27 October 2013 (UTC)

:: Yes, thank for correcting the program.   I had misgivings about flagging it, but it was a very simple change to correct the error, even though it wasn't part of the task's requirements.   (I always appreciate programmers that go the ''extra mile'', even if it's just a few steps.)   I think adding a prime counter to various programs would verify that the program works correctly, at least in factoring composites.   I'm in the process of adding aforementioned code to the REXX program, with proper handling of unity. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]])
