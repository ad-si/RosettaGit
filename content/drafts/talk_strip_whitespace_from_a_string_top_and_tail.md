+++
title = "Talk:Strip whitespace from a string/Top and tail"
description = ""
date = 2018-05-24T17:29:39Z
aliases = []
[extra]
id = 12300
[taxonomies]
categories = []
tags = []
+++

== task clarification==

In the sentence:


 
For the purposes of this task whitespace includes non printable characters such as the space character, the tab character, 

and other such characters that have no corresponding graphical representation. 



Wouldn't it be better to use:

... , and other such characters that '''may''' have no corresponding ...


-- [[User:Gerard Schildberger|Gerard Schildberger]] 17:32, 14 September 2012 (UTC)

Also note that the tab character has a printable character (although, in most circumstances,
it's ''converted'' to a blank (or a number of blanks).  It's graphic on my computer is   <big> '''○''' </big>     (a circle). 

Also, only three characters don't have a printable characters, '00'x, ''the'' blank, and 'ff'x.  Of course, this is very
dependent on the '''code page''' being used. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:06, 17 December 2012 (UTC)

== Error in   '''C'''   code ==

When string is empty, the result is undefined.
