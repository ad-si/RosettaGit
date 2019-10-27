+++
title = "Talk:Sorting algorithms/Sleep sort"
description = ""
date = 2012-06-02T21:09:26Z
aliases = []
[extra]
id = 9949
[taxonomies]
categories = []
tags = []
+++

== Designing for failure? ==

Sleep gives a minimum duration to sleep, but not a maximum.  This algorithm would thus sort incorrectly on a heavily loaded system.  I cannot see a way to design a way around this without cheating on the algorithm.  --[[User:Rdm|Rdm]] 15:14, 22 June 2011 (UTC)
:This is kinda just for fun. I wouldn't worry too much about it. --[[User:Mwn3d|Mwn3d]] 15:31, 22 June 2011 (UTC)

==task query==

Why the requirement that the integers be non-negative?  
 
Does this mean the programs should check if any integers are non-negative?
 
What if the program can handle negative integers?
  
Negative numbers aren't that much of a strange animal to be sorted, and I can't see that any programmer will strain their back coding for such beasts. The little I know of most programmers, weak of back, strong of mind. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:02, 2 June 2012 (UTC)

: This task not only specifies the end result -- that the numbers be sorted -- but the procedure for accomplishing that.  Specifying the procedure is, as a general rule, bad practice, since the implementations are almost invariably inferior to what you get when you simply specify the desired results and consequences.  Mis-specifying is done here on rosettacode, sometimes, I think for the humor value.  Here, for example, a negative integer would require a negative delay -- the code would have to finish waiting before it started waiting.  --[[User:Rdm|Rdm]] 21:08, 2 June 2012 (UTC)
