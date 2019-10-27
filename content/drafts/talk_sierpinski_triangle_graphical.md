+++
title = "Talk:Sierpinski triangle/Graphical"
description = ""
date = 2013-08-28T21:57:57Z
aliases = []
[extra]
id = 9551
[taxonomies]
categories = []
tags = []
+++

== How to added imbedded file ==
Anyone tell me how to add a graphics file as example?  The embedded file widget just gives an example.  It doesn't seem to give me a way to upload the file.

Thanks, --[[User:Dgamey|Dgamey]] 01:32, 6 May 2011 (UTC)
:You can't upload [[Special:Upload|here]]? This link is in the bottom of the sidebar -- "Upload file". --[[User:Mwn3d|Mwn3d]] 01:52, 6 May 2011 (UTC)
:: Thanks - I have to learn to scroll down :) --[[User:Dgamey|Dgamey]] 12:00, 6 May 2011 (UTC)

::: I can't find the "upload file" link in the sidebar. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:57, 28 August 2013 (UTC)

== Additional requirements ==
: Take a parameter N for order (by command line is fine)
: Calculate canvas size to allow for N (up to a limit)

--[[User:Dgamey|Dgamey]] 01:57, 6 May 2011 (UTC)
:: What's all this talk about canvas size for N? What's N got to do with how big the triangle is? --[[User:Ledrug|Ledrug]] 03:31, 5 July 2011 (UTC)
::: First guess: So that no anti-aliasing or sub-pixel considerations are required to draw the image, so that representing the image using a 1-bit palette makes sense, and so such things don't cause an "oversaturation" of the resulting image such that you end up with a large, solid [[wp:Universe of The Legend of Zelda#Triforce|Triforce]] image. --[[User:Short Circuit|Michael Mol]] 15:44, 5 July 2011 (UTC)
:::: If the task has that in mind, it's going to be quite limiting: for pretty much all vector drawing languages/libraries, these are automatically handled when rasterizing, so it's redundant to link N to image size.  Maybe it should be clarified a little. --[[User:Ledrug|Ledrug]] 21:43, 5 July 2011 (UTC)
::::: Strictly speaking only Unicon and TCL speak of canvas size.  The task just says order N.  --[[User:Dgamey|Dgamey]] 01:34, 6 July 2011 (UTC)

== Order ==

I didn't see a reference or anything in the WP article about what order N is.  N appears to be the number of times the triangle is subdivided giving 2^N triangles per side.  Seems obvious to me but it may not be to others.

--[[User:Dgamey|Dgamey]] 12:00, 6 May 2011 (UTC)
