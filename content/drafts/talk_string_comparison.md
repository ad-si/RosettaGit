+++
title = "Talk:String comparison"
description = ""
date = 2013-03-28T01:10:15Z
aliases = []
[extra]
id = 12965
[taxonomies]
categories = []
tags = []
+++

==Promote to task==
Is there any reason why we shouldn't promote this to a full task? It seems clear enough and possible to implement in many different languages. –[[User:Dkf|Donal Fellows]] 22:12, 22 February 2013 (UTC)

== generic comparison and coercive comparison ==

Someone mentioned generic comparison and coercive comparison. We probably need some explanation of what both of those mean.

[[User:Markhobley|Markhobley]] 21:08, 23 February 2013 (UTC)
:How's it now?  The distinction I'm trying to get across is that some operators <em>MIGHT</em> give you string comparison semantics (or some other semantics), while other operators <em>MUST</em> give you string comparison semantics (or fail).  It's an important distinction in some languages, or at least in Perl 6. <tt>:-)</tt>  But even for languages that only give you polymorphic operators (Python) or coercive operators (Perl 5--at least in the absence of overloading), it's still an important semantic difference. --[[User:TimToady|TimToady]] 00:41, 24 February 2013 (UTC)

:if a string operator operates on a non-string it can:
:# Complain about it.
:# Or if the non-string has an agreed string representation it can convert the non-string then work on the string result.
:Some languages do the former, some the latter. (It's also complicated by some languages having only generic comparison operators, i.e. comparing numbers and strings use the exact same operator). --[[User:Paddy3118|Paddy3118]] 10:56, 24 February 2013 (UTC)

:: Excellent. Cheers guys! That looks a lot better. [[User:Markhobley|Markhobley]] 11:19, 24 February 2013 (UTC)

== REXX ==

what is the exclamation mark good for in this line
 !\>> Strictly Not Greater Than
and others?
--[[User:Walterpachl|Walterpachl]] 21:31, 23 February 2013 (UTC)

Right, I'm not sure off the top of my head. It has probably been caused by me because the data that I provide usually comes from my own documentation development wiki that requires the addition of the exclamation mark to suppress the wiki engine from interpreting the following backslash symbol, and I have reformatted the page to a new layout at some point and forgot to remove the exclamation marks that I have previously added. If you know for sure the exclamation marks are wrong then remove them. I have got a manual somewhere, but I can't find it at the moment.  Cheers, Mark.

[[User:Markhobley|Markhobley]] 22:38, 23 February 2013 (UTC)

No biggie; but it's a shame that you chose to use &quot;<tt>!=</tt>&quot; instead of &quot;<tt>\=</tt>&quot; as the '''NOT EQUAL''' operator.  It's only that choice that stopped the program compiling directly to [[NetRexx]] too.  That would have made this one of the few program examples in the [[REXX]] family to cross all dialects.

--[[User:Alansam|Alansam]] 01:10, 28 March 2013 (UTC)
