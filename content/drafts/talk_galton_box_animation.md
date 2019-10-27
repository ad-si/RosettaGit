+++
title = "Talk:Galton box animation"
description = ""
date = 2011-12-28T18:23:45Z
aliases = []
[extra]
id = 9553
[taxonomies]
categories = []
tags = []
+++

== General ==

<strike>: Have an image but need to upload it (not sure how)</strike> --[[User:Dgamey|Dgamey]] 12:09, 6 May 2011 (UTC)
: The task probably needs more specification

--[[User:Dgamey|Dgamey]] 02:09, 6 May 2011 (UTC)

: In a galton box, there are a set of pins arranged in a triangular pattern. A number of balls are dropped in such a manner that they fall in line with the top pin, before moving either to the left or the right of the pin. The ball continues to fall to the left or right of subsequent pins before arriving at one of the collection points between and to the sides of the bottom row of pins.
:: ok to add - good description --[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)

: For the purpose of this task the box should have at least 5 pins on the bottom row, and can be drawn graphically, or using ascii, depending upon the capabilities of the language.
:: also ok to add - good idea  --[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)

The program should simulate the release and arrival of balls, and should continue to release new balls, until there is no more space left to fill the bottom containers (if they are drawn graphically), or until the number in any one container reaches 950.
:: Why 950?? Why full (for graphical)??  Why not just state how many will be dropped and/or the approach?  --[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)

A running total of the number of balls released should be maintained, and a tally of the number of balls for each arrival point should be displayed at the bottom of the screen.
:: Optionally --[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)

It is permissible to show the tally numerically, or as a bar chart or pictogram, depending on the capabilities of the language.
:: also optional--[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)

:: I'm open to it allowing ascii, but is it still an ''animation''?  If the solution is graphical should animation be a requirement or an option?  Should we consider renaming?  --[[User:Dgamey|Dgamey]] 02:43, 13 July 2011 (UTC)
::: You can have animation in ASCII with curses control, or just by asking to "press return after each frame". --[[User:Paddy3118|Paddy3118]] 06:00, 13 July 2011 (UTC)

::: Yeah, my sketch below shows the balls falling. These would be animated. You can see tha the bottom ball lands in container C, on the next frame the ball above it will fall into E or F, and so on. (However, I drew this in notepad, so we have to imagine at this time :P) [[User:Markhobley|Markhobley]] 06:48, 13 July 2011 (UTC)

Here is an ascii sketch that could be used:

             O     Total: 75
 
           O *
 
           * O *
 
         *   * O *
 
       *   *   *   * O
 
     *   *   *   * O *
 
   *   * O *   *   *   *
 
 A   B   C   D   E   F   G
 
 A = 003   B = 005   C = 013   D = 028
 E = 011   F = 002   G = 006

[[User:Markhobley|Markhobley]] 20:08, 12 July 2011 (UTC)

:::Well, 950 fitted nicely with my 3 digit layout above, taking into account some balls are still falling :). You are free to edit what I have written to suit what you want and paste it into the task description. I will leave it with you to ponder over, if you like. [[User:Markhobley|Markhobley]] 05:52, 13 July 2011 (UTC)

:::: Modified the task description.  Let's get some more feedback. --[[User:Dgamey|Dgamey]] 13:01, 13 July 2011 (UTC)

:::: Bwahhh! You didn't use my sketch! :P :P :P [[User:Markhobley|Markhobley]] 13:28, 13 July 2011 (UTC)

== Very cool! ==

I think this is a great task! I will be cooking up an example soon enough.
This is one of the better (IMO) draft tasks here; I don't see why more examples aren't present. --[[User:Crazyfirex|Crazyfirex]] 00:03, 14 September 2011 (UTC)
: Simple. It's quite a bit of work to produce a good implementation, so it keeps drifting down below top priority on peoples' radars. It is majorly cool though. –[[User:Dkf|Donal Fellows]] 09:45, 14 November 2011 (UTC)

:: +1 on your explanation. --[[User:Paddy3118|Paddy3118]] 09:58, 14 November 2011 (UTC)
::: Thanks.

== Oops - examples of image/output needed ==
Sorry folks, my bad, the task says sample output/image.  I'd skipped mine because I used it in the task and set a bad example for the others.  Both C and Autohotkey don't show their output.  I don't have the heart to mark them incorrect for a while.  Also, if a prettier image (like an animated gif) example comes along feel free to replace the original one with that - I'm not that proud.  --[[User:Dgamey|Dgamey]] 03:00, 15 November 2011 (UTC)
:mark them incomplete?--[[User:EMBee|eMBee]] 03:27, 15 November 2011 (UTC)
:: I reached out individually for now.  Give them a chance first. (Is there an incomplete vs. incorrect template --[[User:Dgamey|Dgamey]] 03:34, 15 November 2011 (UTC)
::: All fixed now :) --[[User:Dgamey|Dgamey]] 22:06, 15 November 2011 (UTC)

== Tcl solution ==

I admit it! I just pointed to a solution elsewhere. The source code ''is'' there, as is a picture of the thing in operation, and in my defense it's a really ''great'' implementation that is dropping pictures of balls and not just round dots or characters. There's also a “plink” noise whenever the ball hits a peg. I could copy the code across, but I don't see why I should bother. –[[User:Dkf|Donal Fellows]] 17:29, 28 December 2011 (UTC)
: Preference is to have an on-site solution, even if it isn't as nice a one as is found elsewhere. (I wouldn't be a fan of wholesale copy, anyhow, out of copyright concerns.)  --[[User:Short Circuit|Michael Mol]] 18:23, 28 December 2011 (UTC)
