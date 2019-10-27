+++
title = "Talk:Read a file line by line"
description = ""
date = 2016-06-11T20:44:38Z
aliases = []
[extra]
id = 10124
[taxonomies]
categories = []
tags = []
+++

==C++==
C++ could use an STL-algorithm implemented solution, ala:
http://stackoverflow.com/questions/1567082/how-do-i-iterate-over-cin-line-by-line-in-c/1567703#1567703
----

==NetRexx==
Could someone show the NetRexx Solution? Thanks in advance!
 YOY can't I get used to entering the summary
 Could a prompt be implemented when one saves without summary?
--[[User:Walterpachl|Walterpachl]] 16:20, 8 July 2012 (UTC)


-----

Click on:
 
::::* {option)   Preferences
::::* (tab)   Editing
::::* (checkbox)   <big>[ ]</big>   Prompt me when entering a blank edit summary. 
::::* (button)   Save

-- [[User:Gerard Schildberger|Gerard Schildberger]] 16:31, 8 July 2012 (UTC)

==Visual Basic==
For Visual Basic (vb6), the complex version is in fact very intricate and incomplete. In the end the main part, the input method, the readlinefromfile subroutine is missing?! Therefore I have created the simple version.
-- [[User: PatGarrett|PatGarrett]] ([[User talk:PatGarrett|talk]]) 22:34, 11 June 2016 (UTC)

== Some constraints? ==

I'd like to see some constraints on this task. This task is a potential example of the classic "buffer overflow" problem, and neglecting to even mention that issue in this context seems irresponsible. Perhaps we could impose some reasonable line length limit? (Would a megabyte be a reasonable line length limit?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:06, 8 March 2015 (UTC)
:Only potentially. This task is mature and too late to change. You could however try and write a new buffer overflow task, but we try and make tasks accessible to a wide range of languages and in some, the size of a string buffer may be naturally memory limited rather than limited by some smaller constant that the programmer is steered towards using by their normal language idioms.
: You would have to word it right. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:02, 9 March 2015 (UTC)
