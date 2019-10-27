+++
title = "Talk:Color quantization"
description = ""
date = 2011-08-14T11:44:46Z
aliases = []
[extra]
id = 10308
[taxonomies]
categories = []
tags = []
+++

== file format? ==

Perhaps this task should use the PPM file format, since we know that a lot of languages here support it?  ([[Write ppm file]]) Either that, or perhaps we should first have tasks to read and write .PNG files.  That said, I think the TGA file format would be much simpler to implement support for, than PNG and still supports indexed color formats.  That said PPM could work for this task, though without any compression from the color reduction.  Another alternative might be to simply list the reduced color set and perhaps some associated statistics.  --[[User:Rdm|Rdm]] 19:55, 12 August 2011 (UTC)
: I don't care.  I didn't even ask for the resulting image to be written, so any format you are comfortible with is fine by me, the only required thing is the image content, which is the frog picture. Task requires outputing the colors reduced to, your alternative.  Of course it wouldn't hurt if reduced color image is posted, though. --[[User:Ledrug|Ledrug]] 20:11, 12 August 2011 (UTC)

== J solution ==
Is it possible to change the metric, to put more weight on green, less on red, even less on blue?  The result would probably look better (currently the palette isn't able to resolve the toes under the frog's belly). --[[User:Ledrug|Ledrug]] 23:50, 12 August 2011 (UTC)

:Yes, I should think about that.  Currently, I am using "color distance".  I'll think about other mechanisms.  --[[User:Rdm|Rdm]] 00:07, 13 August 2011 (UTC)

:I have posted a simple approach... perhaps I should also consider using different color representation schemes? --[[User:Rdm|Rdm]] 01:17, 13 August 2011 (UTC)

::I now have four implementations out there, and I have fixed a bug in my initial implementation.  --[[User:Rdm|Rdm]] 01:44, 13 August 2011 (UTC)
::: Without dithering, I think the second palette looks the best.  Kinda funny since that one probably required least amount of work. --[[User:Ledrug|Ledrug]] 05:27, 13 August 2011 (UTC)
:::: That is the case surprisingly often.  Anyways, I have removed the alternate implementations.  If anyone cares, they can find them in the [http://rosettacode.org/mw/index.php?title=Color_quantization&oldid=117230#J edit history for this page] --[[User:Rdm|Rdm]] 11:44, 14 August 2011 (UTC)
