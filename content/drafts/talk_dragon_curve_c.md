+++
title = "Talk:Dragon curve/C"
description = ""
date = 2012-05-21T23:19:58Z
aliases = []
[extra]
id = 10252
[taxonomies]
categories = []
tags = []
+++

==Does it work?==
Can anyone get this thing to compile at all?  It requires code from 6 or 7 different places, and the <code>draw_line</code> is yet nowhere to be found.  Can someone take a shot at this?  Also, judging by the code on this page, the method is pretty silly (saving the whole string is an odd choice).  --[[User:Ledrug|Ledrug]] 00:31, 7 August 2011 (UTC)
: It compil-es(ed) and work-s(-ed). Since this is a wiki code may be changed, fixed, replaced, made better or worse, broken altogether with its original "interface", even without taking attention, care or ask (themselves questions like, in this case, ''why he wrote "to be added to imglib.h"? what does it mean?''). It happens. What you need is of course a function that draws a line. And where can you find it on RC? Maybe here [[Bitmap/Bresenham's_line_algorithm#C]] is a good place to look at, even if a contributor changed the function without asking what the meaning of the header text could have been.
: If you are interested in, long ago I tried to collect these codes to address the problem you're experiencing, but I've not updated it since then, so it could be a little bit or very much out of date. [http://www.mauropanigada.net/filepot/index.php?dir=RosettaCode Here] you can find the archive. About my contributions, despite the fact you like them or not, I assure you I have always compiled and tried my code before posting, even if of course it can be (very likely) bugged and work properly just for the test case required by the task, or few more if I had time to try. --[[User:ShinTakezou|ShinTakezou]] 21:20, 21 May 2012 (UTC)
: Forgot to answer the judgement. L-System are very interesting, and this is just a '''straightforward''' implementation, without lights nor shades, I believe. Saving the whole string (?) is what it happens when the focus is not on efficient string handling, but rather on the "formal essence" of the system (which, by the way, at the time t is "described" by the whole string). --[[User:ShinTakezou|ShinTakezou]] 21:33, 21 May 2012 (UTC)
:: Ultimately, the preferred metric on RC for whether a piece of code is good or not is founded on two principles: ''does it work'' (i.e., does it actually solve the task) and ''is it idiomatic'' (i.e., is the code written in the way that practitioners of the language would recommend, given that they're attempting the task in the first place). Everything else (including whether the code is using an “interesting” technique) is of lesser importance. IMO, using a library (even if a custom one) is very idiomatic for a C program. –[[User:Dkf|Donal Fellows]] 23:19, 21 May 2012 (UTC)
