+++
title = "Talk:Search a list"
description = ""
date = 2016-08-05T11:46:10Z
aliases = []
[extra]
id = 3148
[taxonomies]
categories = []
tags = []
+++

Is it really essential that the sequence of strings is in an array? For some languages, other data structures may be more suitable. Indeed, the Common Lisp example uses a list instead of an array, and therefore doesn't conform to the current task description. Also note that the task's title speaks of a list, but the task description speaks of an array, which is sort of a contradiction. I think it would be better to allow the examples for each language to use any data structure for the sequence, as long as it's directly provided by the language or standard library. --[[User:Ce|Ce]] 13:33, 26 November 2008 (UTC)
:I don't think the operative word is "list", but "index" instead. As long as the collection innately supports access at an index it should be fine.--[[User:Mwn3d|Mwn3d]] 14:24, 26 November 2008 (UTC)
:: What does it mean to "inately support access at an index"? Of course for any sequence you can define access at index ''n'' by just starting at the first element and then going to the next element ''n'' times (this gives zero-based indexing, of course). Note that in Lisp, the function <code>nth</code> does exactly that for lists. However, for a real array, accessing the element at index ''n'' is a constant-time operation. Therefore, would you say that Lisp lists (as opposed to Lisp vectors) "inately support access at an index"? And if so, what would a sequence look like that doesn't do so?
:: Also, does it really matter for the task that you can access the element through that index? After all, the indexing itself isn't demonstrated. I guess the question comes down to what the task is actually intended to demonstrate. --[[User:Ce|Ce]] 16:47, 26 November 2008 (UTC)
:::You may be right. The collection type is obviously not the focus of this task, so it shouldn't be restricted if collections other than arrays fit the bill. It shouldn't be restricted to "haystack[i]" because first, some languages don't use that syntax and second, some collections which do support indexing do it through functions and methods (like nth from List or ArrayList.get() from Java). It should work for collections where if I run this function and get an index, then add an element to the "end" of the collection (that is, add an element to an end of the collection so that its index is greater than the index I got when I ran the function), then run the function again with the same arguments, I should get the same number. If that sounds like an OK specification, then which collections match it? --[[User:Mwn3d|Mwn3d]] 16:59, 26 November 2008 (UTC)
:I have changed the wording to indexable ordered collection which would include arrays, lists, tuples, hashes/dictionaries ... and removed access syntax which does not apply in some manguages. I hope it is suitably generic. --[[User:Paddy3118|Paddy3118]] 06:46, 27 November 2008 (UTC)

:P.S. I couldn't help extending the task too. --[[User:Paddy3118|Paddy3118]] 06:46, 27 November 2008 (UTC)


==Erlang==
lists:dropwhile/2 should be used probably, to simplify traversing list.

==What???==

Only a Java programmer could have come up with such a ridiculous task description.
: How about doing something useful?  Uttering insults while hiding in a corner helps nobody (the insult wasn't even accurate). --[[User:Ledrug|Ledrug]] 23:24, 19 September 2011 (UTC)

== REXX Version 3 ==

"/*underscores (_) are used to NOT*/

/*  conflict with variable names.*/"

Using '0' instead of '_' would be safer. _gold is a valid variable name, 0gold is not.

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:57, 20 August 2015 (UTC)

-----

The full quote from the original statement is:

:: ''This method pre-prends an underscore (underbar) to avoid collision with any REXX variable names''.   ''Therefore, there shouldn't be any REXX variable names (in this program) that have a leading underscore   (_)''. 

The underscore   (_)   was chosen to make the REXX code more readable, 

it was not claimed that   ''_gold''   wasn't a valid REXX variable name.

The use of a pre-pended zero makes the elements harder to read/peruse.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:19, 25 August 2015 (UTC)

==Raising an exception?==

The task description says "''Raise an exception if the needle is missing.''" - however, many of the solutions don't do this. Instead, they rather print a string like "needle not in haystack" to STDOUT in this case and resume the program. The Python solution, for example, even goes out of its way to ''suppress'' the exception that would naturally be thrown. Also, some languages don't even <i>have</i> exceptions.

I propose changing that sentence of the task description, to: "''Print an appropriate message if the needle is missing.''" Any objections?

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:14, 5 August 2016 (UTC)

* It's a good suggestion, and the reference to exceptions was probably always a bit parochial - exception-raising is more relevant to imperative languages and coding idioms than to function composition and functional languages. (A functional language might more typically, for example, use an option type (Maybe etc) and pass the invalid/undefined status of a result up through the composition chain). Hout
