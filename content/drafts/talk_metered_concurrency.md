+++
title = "Talk:Metered concurrency"
description = ""
date = 2018-10-15T18:21:47Z
aliases = []
[extra]
id = 2023
[taxonomies]
categories = []
tags = []
+++

==Table of contents==
It looks like this page lack a table of contents. Other pages have it, but this doesn't. I can't seem to find a way to add one either... Perhaps someone could add it/shed some light on the issue? --[[User:Ullner|Ullner]] 13:30, 6 April 2007 (EDT)
:Ah, according to [http://en.wikipedia.org/wiki/Wikipedia:Section#Table_of_contents_.28TOC.29 Wikipedia's help section], the page must contain more than 3 headings to get an automagic TOC. And this page only got three. So, it seems this will be fixed automagically once someone add a forth language. --[[User:Ullner|Ullner]] 11:05, 8 April 2007 (EDT)  

:: You can force a table of contents   (TOC)   by using the string: 
         <big> <nowiki> __TOC__ </nowiki> </big> 
:: which is a seven character string:   (underbar)(underbar)(3 uppercase letters of TOC)(underbar)(underbar).   Another name for '''underbar''' is '''underscore'''.   You can always remove the   <nowiki> __TOC__ </nowiki>   after more computer programming languages are added.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]])

==The C code==

I wrote the C code and I think it behaves like the Java one (except I had to implement synchronization myself), but I am not sure... and I am not sure to have understood the task, ... this is my first use of the pthreads, and first non-unique thread program (my normal use of a language is single threaded, even though I used simple semaphore to lock resources...)

So I added the template ''incorrect'', maybe ''needing review'' is better, but the template says because of a change in the task... and it is not the case. Simply I would appreciate to have a check on my code, since I am not so sure of it. Of course, I've written it to solve this task exactly as I've read it today: [[User:ShinTakezou|ShinTakezou]] 17:14, 9 December 2008 (UTC)
