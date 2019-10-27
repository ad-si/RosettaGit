+++
title = "Talk:Leap year"
description = ""
date = 2012-03-12T19:11:15Z
aliases = []
[extra]
id = 10624
[taxonomies]
categories = []
tags = []
+++

== "in the Gregorian calendar" ==

During 2011 August 13, Markjreed [http://rosettacode.org/mw/index.php?title=Leap_year&diff=117353&oldid=117345 amended the task] to specify "in the Gregorian calendar". This contradicted the Ruby example, which already used the Julian calendar for years before 1583. I have now added an alternate Ruby example that uses proleptic Gregorian calendar. --[[User:Kernigh|Kernigh]] 18:52, 6 October 2011 (UTC)
: There used to be a lengthy discussion here about the leap year rules in effect that led to that change.  What happened to the history of the talk page? --[[User:Markjreed|Markjreed]] 19:11, 12 March 2012 (UTC)

== Faster implementation ==
After some experiments with gcc, I suggest to write, instead of the usual version :
   (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 16 = 0))
That way there is only one division to do. Even though gcc uses ''imul'' to do that, it's still shorter.
And probably any compiler will optimize better this code. Okay, it's probably not *that* important to optimize Is_Leap_Year !
[[User:Nochnix|Nochnix]] 11:06, 13 February 2012 (UTC)
