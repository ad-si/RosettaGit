+++
title = "Talk:Write language name in 3D ASCII"
description = ""
date = 2014-10-03T07:16:43Z
aliases = []
[extra]
id = 9536
[taxonomies]
categories = []
tags = []
+++

== Criteria for non-draft ==

The problem with this task overall is that it has only one implementation in about 5 months of existence. While a task ''may'' have fewer than 4 implementations at the start of its life, it should normally get above that threshold rapidly (i.e., timescale: days). My concern is that this “task” (without admitting whether it is a task or not) is never going to attract any further implementations because of the way it is specified; very few languages/runtimes have a library for this sort of thing on the grounds that it's not actually very useful…

A general “write the language's name in 3D” task would be far better, since that would admit graphical implementations as well. –[[User:Dkf|Donal Fellows]] 15:11, 5 May 2011 (UTC)
:I agree. This task has been sitting around for a long time with nothing being done to it. While it is "cute" it seems that it isn't interesting enough. I think opening it up to other methods of display (built-in graphics, OpenGL, etc.) would be good. --[[User:Mwn3d|Mwn3d]] 15:34, 5 May 2011 (UTC)

:We could always create a separate task for graphics and opengl, if necessary.
:--[[User:Markhobley|Markhobley]] 22:33, 3 June 2011 (UTC)

The task description appears complete to me. I don't think lack of popularity is a reason to keep the task as a draft. Some tasks are just more less interesting than others. If there are no issues with the task description, then I suggest that we promote this to task.
--[[User:Markhobley|Markhobley]] 22:33, 3 June 2011 (UTC)
:Lack of popularity, to me, means that the task description hasn't really been tested much. I like to wait until there are a few different languages implementing the task from a few different users. That at least proves that several people understand and like the description. --[[User:Mwn3d|Mwn3d]] 23:48, 3 June 2011 (UTC)
:: From my perspective, the issue is that I don't see this task as being at all ''relevant'' to anything. “3D in ASCII” (or even Unicode) is just of enormously low value. (Showing 3D text in a graphical display is of value by comparison; a number of applications do that.) It also happens to be substantially more awkward for most languages to do than the exemplar (I suspect that “Smalltalk” will have problems fitting in a standard display!) so I'm guessing that the vast majority of people are never going to be keen on implementing this task. Plus right now the only thing being demonstrated is loading and calling into a library (that other languages don't have a near-equivalent of); big deal, other tasks do that better. –[[User:Dkf|Donal Fellows]] 10:38, 16 June 2011 (UTC)
:::I've been musing on-and-off on doing it as an isometric-view voxel renderer, and using a character->voxel set mapping. I doubt I'll ever get around to it, though. It's not a high priority, but has interesting possible approaches. Perhaps if the task were distilled to specifying an approach with interesting components, it would be more popular? --[[User:Short Circuit|Michael Mol]] 15:49, 16 June 2011 (UTC)

:::: A way that I have done this in the past is to just use flat coloured spaces to create the letter shapes, repeating the letter drawing algorithm one square down and one square to the right in a lighter colour. This causes a shadow effect behind the lettering, giving it a 3d appearance. A reimplementation of that would fit this task nicely. --[[User:Markhobley|Markhobley]] 16:53, 16 June 2011 (UTC)

<general rant>
This kind of tasks (draw something) are always either mundane, or more about libraries than the language itself--unless the language is meant to do graphics, but then the tasks often say "ascii art".  One of the most silly IMO is [[Draw a cuboid]]: look at the ascii outputs, and tell me you really think they are visually 4x3x2.

I think each task should focus on one thing, say [[Sierpinski triangle]] should not limit output to be ascii because it's more about recursion and factals; same goes with this one, let people be creative within their languages while keeping a theme going.  You can hardly motivate anyone to draw ascii art with POV-Ray, Logo or PostScript. --[[User:Ledrug|Ledrug]] 18:03, 16 June 2011 (UTC)

:I think character based and graphical tasks should be separated. Tasks implemented on graphical user interfaces may not be usable on a character terminal, so a graphical implementation would not provide a suitable solution for a task designed to run on a character based terminal. However, a character based implementation may provide a replacement for a graphical one, so ascii art could be used to provide an implementation of a graphical based task (allowing implementations on platforms or languages that do not support graphics to be provided). --[[User:Markhobley|Markhobley]] 18:37, 16 June 2011 (UTC)

:I don't think that we need to worry that the Pov-Ray, Logo and Postscript developers have not yet been motivated to implement an ascii art solution. We are not working to any urgency here. There are lots of tasks that have not been implemented in many languages. --[[User:Markhobley|Markhobley]] 18:37, 16 June 2011 (UTC)

== Removed obfuscated Perl and perl6 code==
Obfuscated code does not aid language comparison. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:16, 3 October 2014 (UTC)
