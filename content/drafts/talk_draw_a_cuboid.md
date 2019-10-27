+++
title = "Talk:Draw a cuboid"
description = ""
date = 2011-06-17T17:21:55Z
aliases = []
[extra]
id = 8977
[taxonomies]
categories = []
tags = []
+++

What about those of us without 3D displays? (What about those of us with them? :-)) What are we supposed to actually do here? If we're projecting onto a surface, should that projection be static or permit rotation of the object or motion of the field of view? What is the output supposed to look like (attach a screenshot…) –[[User:Dkf|Donal Fellows]] 13:18, 14 December 2010 (UTC)


The task is to draw a cuboid within the language capabilities. A 3D dislay is not required here. The cubiod can be represented graphically, or in ascii art, depending on the language capabilities. As long as we have three faces visible, then I think that fulfils the criteria of being a cuboid. Either static or rotational projection is acceptable for this task. The examples provided so far seem to fulfil this criteria.

[[User:Markhobley|Markhobley]] 22:50, 20 December 2010 (UTC)

:Because of the textual nature of RC and the web in general, I suggest the task be "render (some object) as three dimensional ASCII art".  
:I think it'd be fun if (some object) be (your language's name)

but that's because we Jers happen to have such a solution already, that I would like to advertise.  (Also because "J" looks cool in 3D. :)
:--[[User:DanBron|DanBron]] 17:24, 15 December 2010 (UTC)

Yeah, that looked good. I moved that to a separate task, because it went beyond drawing of a cuboid.

[[User:Markhobley|Markhobley]] 22:50, 20 December 2010 (UTC)


The PicoLisp version needs to be translated to C or Java.


So...what about those of use who don't ASCII terminals, but only Unicode terminals?  <tt>:-)</tt>  I've intentionally put some FULLWIDTH chars into the perl6 solution because it looks better.  It doesn't seem quite right to refer to Unicode art as ASCII art anymore, so I suggest we start retiring the term ASCII art in favor of Unicode art.  --[[User:TimToady|TimToady]] 00:55, 17 June 2011 (UTC)

::We can still use the term "Ascii Art", because the artwork is made of characters that appear in the Ascii table, and can be displayed on an Ascii compatible terminal (We are talking about the appearance or shape of the characters here, not the numeric codes). The terminal may use different codes for the characters, but that does not matter. I would still say that the artwork is still Ascii art, because it could be represented as such by transcoding. --[[User:Markhobley|Markhobley]] 12:46, 17 June 2011 (UTC)
:::So you're talking about explcitly limiting the charset used to printable characters in the range of 0-127? That omits [[wp:Box-drawing characters|Box-drawing characters]] and the [[wp:Miscellaneous Symbols|Miscellaneous Symbols]] range. The "FULLWIDTH" chars Larry is talking about come from the box-drawing set. (Specifically, U+2571, which I don't think was in IBM's [[wp:Code page 437|Code page 437]], either) --[[User:Short Circuit|Michael Mol]] 16:45, 17 June 2011 (UTC)

::::We can still provide an implementation and just attach a saying that a Unicode terminal is
required in order for it to be utilized. --17:21, 17 June 2011 (UTC)

:Heh it does look better in my xterm, but looks terrible in the browser.  I think firefox decided to use some proportional font for those wide chars because my default monospace font doesn't have them.  Also, the first cuboid still doesn't look anywhere near 2x3x4 :) --[[User:Ledrug|Ledrug]] 01:00, 17 June 2011 (UTC)
:"glyph art"? --[[User:Short Circuit|Michael Mol]] 11:43, 17 June 2011 (UTC)
