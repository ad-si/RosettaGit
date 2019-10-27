+++
title = "Talk:Pinstripe/Printer"
description = ""
date = 2011-07-10T07:31:33Z
aliases = []
[extra]
id = 10053
[taxonomies]
categories = []
tags = []
+++

== Clarify? ==

What's a "point"? Smallest possible ink smudge? 1/72 inch? Also there are only two implementations, no consensus request, no notification, how did this task get out of draft? --[[User:Ledrug|Ledrug]] 23:59, 7 July 2011 (UTC)
::Yes, the smallest possible ink mark. The printers equivalent of a pixel. [[User:Markhobley|Markhobley]] 08:20, 8 July 2011 (UTC)

I'm not sure why the examples are marked as incorrect. They look ok to me, but I am not an expert on those languages, (and I have not tested them for operation). [[User:Markhobley|Markhobley]] 09:57, 8 July 2011 (UTC)
:Because both are using the "point" as in 1/72 of an inch, which decidedly is not the same as a smallest possible ink smudge.  It is much more reasonable to do on a modern printer, because determining the smallest dot size of a printer reliably and portably is nontrivial, and printer languages such as PS and PCL exist precisely so people don't need to worry about this kind of things -- but, that's not what your task asks for. --[[User:Ledrug|Ledrug]] 23:24, 8 July 2011 (UTC)

::In the tcl comments it reads "Send postscript to default printer, scaled 1 pixel -> 1 point". Isn't that achieving the goal? Or does the code not actually do what the comment says?

::I am not familiar with PS or PCL. Do these go down to fractions of a point? [[User:Markhobley|Markhobley]] 23:50, 8 July 2011 (UTC)

::Don't printer drivers have the facility to return the smallest dot size or resolution information that we could use to calculate the smallest dot size? [[User:Markhobley|Markhobley]] 00:04, 9 July 2011 (UTC)
::: The TCL code, as far as I can see, is scaling 1 screen pixel to 1 pt on printout.  1pt is quite a bit larger than a smallest dot on modern printers. In general you can't tell what the smallest dot size is unless you know the exact printer model or you have a driver that knows that.  Keeping track of all printer models and specs is nuts, let's ignore that.  When you say "driver", a) a printer doesn't necessarily have a driver; b) it could be a generic postscript driver that knows nothing about device resolution; c) the printer might not be anywhere near your computer: what about network printers that expose only an http upload interface, for example?  And, "printer resolution" is ill defined to begin with.  Some printers boast say 2400 dpi resolution, but that's only when printing raster images with some clever halftoning trick, while the true resolution for a black dot may be only 600dpi.  An inkjet printer has a fixed nozzle movement resolution, but each dot made by the nozzle depends highly on the ink and paper quality, which can be a much bigger smear.  If your printer is one of those ancient analog x-y plotter, its resolution may be solely determined by how worn out its stylus tip is, no driver can help you with ''that''.
::: As I said before, in modern printers, you generally don't (and can't) worry about resolution as long as it's good enough.  I'm not sure what kind of printer you had in mind with this task, but have the feeling that the task is not really thought through.  Which leads to my other question: how is this task not a draft? --[[User:Ledrug|Ledrug]] 02:07, 9 July 2011 (UTC)
:::: The task was drafted, people improved it. I waited a month for feedback. No more came, I saw no problems, and we had some implementations, so I undrafted it. We can always knock it back to draft again, if there is a problem.
:::: It would be an acceptable solution to use the smallest marks that the language provides, rather than the printer provides, if this makes a difference. I do want to go too far down the road of "subpixel rendering" here, so true resolution is also an acceptable solution here. [[User:Markhobley|Markhobley]] 09:09, 9 July 2011 (UTC)
::::: There's no "smallest marks" provided by the language, at least not in postscript.  You can draw a rectangle as small as the floating number precision the VM allows, and the language spec has something about how a renderer is supposed to rasterize it (if size is not zero, ''something'' should be drawn).  If you have a PS printer, you could draw a line that's as thin as possible in some sense (0 setlinewidth), but you can't tell how much to move forward to draw the next line so that it's exactly "1 pixel" apart -- and why would you ever want to do that? For a printout, you always care more about how big it is physically in terms of inches and milimeters, and that's why PS and PCL (and PDF) are vector based page description.  On a modern printer, you don't quite control if it does "subpixel rendering", and it normally doesn't need to: you have to sqint pretty hard to see a 600dpi dot, it's pointless.  Really, this task serves what purpose exactly?  Could you outline it here? --[[User:Ledrug|Ledrug]] 02:13, 10 July 2011 (UTC)
::::::The purpose is to demonstrate how to position the dots on the printer. The idea being, if you are able to produce the test pattern, then you have control of the printer. You can then adapt this technique for other printing tasks. The variation in the test pattern, enables you to gauge the usability of the printer in terms of effectiveness of the dot spacing. We probably need additional patterns for vertical spaces (using horizontal lines), but I didn't want to overcomplicate the code on here, so I decided to use separate tasks for these different elements. [[User:Markhobley|Markhobley]] 07:31, 10 July 2011 (UTC)
