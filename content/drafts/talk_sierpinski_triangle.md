+++
title = "Talk:Sierpinski triangle"
description = ""
date = 2016-05-12T23:06:16Z
aliases = []
[extra]
id = 2736
[taxonomies]
categories = []
tags = []
+++

I'm confused about the algorithm in the JavaScript version. Could we get more descriptive variable names for u and t or maybe a pseudocode algorithm that everyone can use? --[[User:Mwn3d|Mwn3d]] 16:29, 14 March 2008 (MDT)

== Graphics lib ==

Would it be appropriate to redefine this task to use an assumed function named SetPixel(), as we do in a number of other tasks? --[[User:Short Circuit|Short Circuit]] 11:53, 1 May 2009 (UTC)

* That would exclude all the languages without 'bitmap storage' solutions. Also, defining it textually has the advantage that people can grab and run the code, rather than having to add libraries from elsewhere.<p>I like the idea of having graphical output ''if'' the code to do it is nice; but I think we should focus on having clear readable examples so that they are enlightening to those who do not or only barely know the languages, and beware of putting accidental complexity in the tasks that obscures the fundamentals (in this case, the chosen algorithm for constructing the points of a sierpinski triangle). --[[User:Kevin Reid|Kevin Reid]] 12:08, 1 May 2009 (UTC)

:: I think converting it into graphics would be rather simple, even without a really existing function. It is enough to advance x on every space, and set pixel on every *, and advance y on every "\n" or similar... Anyway... what about languages (like Postscript, Metafont...) which have a "natural" graphics nature? Or what about using the codes in the [[Raster graphics operations]] for languages that implemented that, and &mdash;also?&mdash; a certain common library/package for the others, if (easily) available? --[[User:ShinTakezou|ShinTakezou]] 10:54, 2 May 2009 (UTC)

== Variations ==

How about a related task to draw the gasket using the [[wp:chaos game]]? Or the Xor method mentioned in [[wp:Sierpinski triangle|the Wikipedia article]]? I've done both in the past; pretty simple, really. -- [[User:Eriksiers|Eriksiers]] 00:51, 4 November 2009 (UTC)
: http://rosettacode.org/wiki/Chaos_game
