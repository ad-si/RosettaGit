+++
title = "Talk:Loops/Continue"
description = ""
date = 2011-05-30T18:31:33Z
aliases = []
[extra]
id = 3301
[taxonomies]
categories = []
tags = []
+++

== Modula-3 ==

Was the Modula-3 example supposed to have the word "RAISE" in it somewhere? --[[User:Mwn3d|Mwn3d]] 20:54, 9 January 2009 (UTC)

Er, whoops, meant to write RETURN, fixed now :P --[[User:Mbishop|Mbishop]] 21:10, 9 January 2009 (UTC)

== Shouldn't this made more explicit? ==

The task asks just for an output, does not force nor suggest how to achieve it. Yes, one can take a look at written example, but if all we need is to compliant to task requirements, than the Loop/Continue feature could be missed! Shouldn't the task contain a sentence like "forcing the next iteration within the loop"? So I've done it (I'm feeling bold in modifying the wiki today:D) --[[User:ShinTakezou|ShinTakezou]] 11:54, 27 April 2009 (UTC)

== Poor example? ==

Isn't this a poor example of continue, since it is so easily completed using an if-else instead? How about a task which is more of an 'early-exit' sort of situation? --[[User:Kevin Reid|Kevin Reid]] 16:40, 16 May 2009 (UTC)
:"Early exit" sounds more like break to me. The task says to force the next iteration. I think that's enough. --[[User:Mwn3d|Mwn3d]] 16:45, 16 May 2009 (UTC)

== Fortran 77! ==

Fortran 77 can do this loop (as well as most of the others), but it cannot print without creating a newline. Some compilers have their own way of doing that, but it is simply not a feature in the ANSI FORTRAN 77 standard. On the other hand, F77 basically doesn't have loops, so I feel examples of rigged-up loops would be particularly helpful. If I am going to post examples, they will either (a) not meet your program specifications or (b) not meet ANSI's specifications.

For the former, I'd probably use a double-newline or something to indicate the difference. For the latter, I'd post a warning about that feature and indicate nonstandard code.

Personally, I feel it would be better to go with the latter because, in this specific case, we are talking about literally one nonstandard character. But I defer to your judgment. --[[User:Maaatt|Maaatt]] 19:24, 15 April 2011 (UTC)
:Whichever way is fine as long as it's noted. We can examine particular examples on an ad hoc basis if it looks like they go too far away from the task specs. An incorrect example that is close (as long as it's marked incorrect) is probably better than no example at all. --[[User:Mwn3d|Mwn3d]] 19:58, 15 April 2011 (UTC)

I think it makes little sense to force every iteration. It would be better to maybe force just one iteration to skip. For example, skip the number six, giving output as 1,2,3,4,5,7,8,9,10.

[[User:Markhobley|Markhobley]] 18:31, 30 May 2011 (UTC)
