+++
title = "Talk:Percentage difference between images"
description = ""
date = 2011-09-02T13:11:41Z
aliases = []
[extra]
id = 3113
[taxonomies]
categories = []
tags = []
+++

== examples ==

Please provide sample inputs and outputs. --[[User:DanBron|DanBron]] 23:34, 13 November 2008 (UTC)

: Any image can be used, open it in Gimp and save it with 2 different jpeg compression ratio.
: Then you have img1.jpg and img2.jpg, so call this script this way (followed by its output):
: ./script.ml img1.jpg img2.jpg
:  diff: 0.004283 percent
: [[User:Blue Prawn|Blue Prawn]] 00:01, 14 November 2008 (UTC)
::Some of us don't have Gimp.  Having images would also make the article/task more aesthetically appealing.
::--[[User:DanBron|DanBron]] 15:04, 14 November 2008 (UTC)

::: OK, I do understand. Just give me the permition to upload images. As long as I have understood one need to have the bureaucrate statut. Are you able to give me this statut? If not, who have I to ask to get it?
::: [[User:Blue Prawn|Blue Prawn]] 08:13, 15 November 2008 (UTC)
::::I have pictures on my computer at 100 and 50 but file upload is broken at the moment. I will add links to the task when I get them uploaded. --[[User:Mwn3d|Mwn3d]] 03:07, 16 November 2008 (UTC)
::::Could someone run their program with the test images and put the expected results in the task description? --[[User:Mwn3d|Mwn3d]] 19:49, 4 December 2008 (UTC)
:::::There you go. [[User:Drea|Drea]] 20:19, 4 December 2008 (UTC)

==Disagreement research project==

The different examples produce slightly different results. It would be good to collect what the different results are (taking into account different precisions in output) and what kind of arithmetic each such example uses, and figure out whether particular examples are actually wrong.

The arithmetic properties I can think of are:
* integer or floating-point color components
* integer or floating-point sum
* when division by image size is done
* when division by maximum color component value (if not 1.0, e.g. 255) is done

--[[User:Kevin Reid|Kevin Reid]] 20:03, 29 July 2009 (UTC)

: Some of the larger differences likely come from the jpeg decoding.  I got 1.78 at first (using djpeg.)  I tried the -fast option and it went to 1.84.  Switching to convert dropped the answer to the 1.62 range most people were getting.  &mdash;[[User:Sonia|Sonia]] 05:16, 23 June 2011 (UTC)

: Be careful when you test.  If you grab the images directly from the task page you will get 200x200 px versions and get numbers around 1.5.   I'm adding the correct file links to the task description to avoid this little oops. --[[User:Dgamey|Dgamey]] 22:00, 1 September 2011 (UTC)

==Python Image Library?==
Where is the <code>Image</code> library from? --[[User:Paddy3118|Paddy3118]] 12:56, 31 December 2009 (UTC)
:[http://www.pythonware.com/products/pil/ Here], I think. PIL has a module named <code>Image</code>, at any rate. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:08, 31 December 2009 (UTC)

== Clarify "Difference"? ==

How come no one ever asked what the definition of difference between images is?  Sample programs seem to be calculating sum of abs(R-R') + abs(G-G') + abs(B-B'), which is a useless quantity in image processing. --[[User:Ledrug|Ledrug]] 03:57, 27 July 2011 (UTC)
: I wondered about this.  It's okay as a simple benchmark / test I guess.   
:: I actually liked the example (REBOL) that produced the differenced image.  That would have been a bit more interesting.  Unfortunately the author didn't explain what the code that's enhancing the contrast is doing at the pixel level so not too many of us could actually repeat it if we had to code it --[[User:Dgamey|Dgamey]] 22:12, 1 September 2011 (UTC)
::: Simpliest way to do contrast is find min and max pixel values, then rescale all colors such that min is 0 and max is 1 (or 255).  If you want to be fancier, you can also gamma correct it, which is a little questionable. --[[User:Ledrug|Ledrug]] 02:10, 2 September 2011 (UTC)
:::: I'd thought of that, but one white and black pixel forces something else. Scaling by some number of std dev from mean perhaps.  Maybe based on luminance.  I was thinking that if a task to produce a difference image and intensified one would need to have a comparable methodology.  Or produce the diff image and explain the intensified one.  Something like that.  But I digress. --[[User:Dgamey|Dgamey]] 13:11, 2 September 2011 (UTC)
