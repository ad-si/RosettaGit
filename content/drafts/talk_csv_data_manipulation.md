+++
title = "Talk:CSV data manipulation"
description = ""
date = 2016-11-18T22:40:24Z
aliases = []
[extra]
id = 14432
[taxonomies]
categories = []
tags = []
+++

==Save back to the same file?==
Hi. Why is that a part of the task? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:24, 21 June 2013 (UTC)
: And why have I been the only person to implement it so far (as far as I can tell)? –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 13:52, 27 June 2013 (UTC)
: You are right, there is no purpose for it and it has been removed. --[[User:Vilipetek|Vilipetek]] ([[User talk:Vilipetek|talk]]) 22:14, 8 July 2013 (UTC)


== Vague task description ==
The task description is vague. It just says "change some values". What values?
I think it should be specified more clearly so that the different implementations would be comparable.
Or, at least, there should be description on each implementation of what that implementation does.
--[[User:PauliKL|PauliKL]] ([[User talk:PauliKL|talk]]) 12:11, 7 October 2013 (UTC)

== Fortran ==

I'll leave the section on old Fortran, but as seems to be customary for the author, this is a loooong section completely overlooking modern Fortran features. He seems not to be aware that the standard has seen some revisions since Fortran 66. Besides, he is also completely overlooking the fact that to read CSV without messing up with edit descriptors, one would ''obviously'' read a line and parse the CSV. Here I'm doing this minimally, since I don't want the program to become too long. But the general idea is: with Fortran 2003, all of this is easy stuff. Contrary to what Dinosaur wrote. Again. Sigh.
[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 22:32, 18 November 2016 (UTC)
