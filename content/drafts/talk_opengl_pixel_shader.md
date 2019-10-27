+++
title = "Talk:OpenGL Pixel Shader"
description = ""
date = 2012-01-08T22:35:24Z
aliases = []
[extra]
id = 10723
[taxonomies]
categories = []
tags = []
+++

==Issues?==

Does anyone see any major problems with this task proposal? --[[User:Rdm|Rdm]] 18:05, 25 October 2011 (UTC)
: Does GLSL have a built-in random number generator?  If not, a linear gradient may be easier (and easier to verify) than a random RGB shader. --[[User:Ledrug|Ledrug]] 20:06, 25 October 2011 (UTC)
:: There is a noise() function, but apparently it's not supported very well.  However, we do not need high quality random numbers here -- modulo and arc tangent using old pixel value and coordinate values could probably be made into something adequate.  I would like to avoid a gradient, however, because we already a gradient in [[OpenGL]]. --[[User:Rdm|Rdm]] 03:17, 26 October 2011 (UTC)
::: Assuming you want to procedurally generate color values based on coords in model view space, "random" gives the impression that you want the generated values to be very noisy, i.e. have a high frequency.  Broadly speaking, as long as the so generated texture is deterministic, noise is just a kind of gradient.  The problem of the word "random" is that, firstly, the code still needs to decide what frequency to use: pixel shader gets called on each pixel on every scanline, which is not directly coupled to model view coordinates due to projection transformations.  Secondly, if the frequency is too high (comparable to screen dpi under current projection), it will suffer greatly from aliasing, whereby any movement along z or any rotation will cause pixels to sample to completely different values, resulting in apparently changing texture.  Coherent psuedo random texture with frequency cutoff is also difficult (I have a Perlin noise example in the draw sphere task).  Granted, it might not be a problem because most implementations will probably just draw something static and never rotate it, but it's still easier to verify the code is doing the right thing if it's asked to draw something identifiable, say a checker pattern, stripes, concentric circles, etc. --[[User:Ledrug|Ledrug]] 03:50, 26 October 2011 (UTC)
:::: Yes, I want noise, and I want the noise for any one pixel to tend to change significantly from frame to frame.  I do not care much about artifacts if these conditions are met.  I do not care if one's mental model allows them to see this information as deterministic.  Does this help?  Do you have any suggested changes in the wording, with this in mind?  Thanks.  --[[User:Rdm|Rdm]] 12:40, 26 October 2011 (UTC)
::::: In that case, the pixel shader is actually simpler than usual because coordinates have no bearings on the color.  If this is the intention, probably mention it in the optional goal #1, as it isn't clear if "rerendering" should result in exactly the same pixel colors. --[[User:Ledrug|Ledrug]] 12:02, 27 October 2011 (UTC)
:::::: Yes, the point of this was simplicity, something like a "hello world" that lets you make sure you know how to use your build tools.  That said, I want to phrase this distinction slightly differently (because you need to incorporate the coordinates otherwise you have no way of achieving different colors at different coordinates).  Anyways, I'll try an update.  Thanks.  --[[User:Rdm|Rdm]] 14:56, 27 October 2011 (UTC)
: Shouldn't GLSL be added as a new [[:Category:GLSL|language]] and this task added to it? [[Special:Contributions/89.243.198.247|89.243.198.247]] 16:14, 8 January 2012 (UTC)
:: No.  
:: I do not have anything against GLSL being added as a language (though given its constraints, I doubt many tasks could be addressed using GLSL).  But the purpose of this task is to deploy a "Hello World"-ish GLSL program.  And that's not something you can do from within GLSL. --[[User:Rdm|Rdm]] 22:35, 8 January 2012 (UTC)
