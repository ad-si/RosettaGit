+++
title = "Talk:Perlin noise"
description = ""
date = 2016-06-16T20:48:59Z
aliases = []
[extra]
id = 17327
[taxonomies]
categories = []
tags = []
+++

==Correctness==
The D implementation generates exactly the same outputs of the Java version. I have used the D code to generate this image (with the grayscale code visible in the D entry): http://oi62.tinypic.com/2pt9flx.jpg

Is such image correct? Isn't a correct Perlin noise image without sharp borders, like this?

http://4.bp.blogspot.com/-u4wfD21sIvU/UsI85U78XYI/AAAAAAAAAx8/NG2fEzYjTUA/s1600/perlin2.png
-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

:Your image is not correct.  The perlin noise has a range between -1 and 1, and you use this to convert it into an unsigned byte:

:<code>Gray(cast(ubyte)(p * 256))</code>

:Try this:

:<code>Gray(cast(ubyte)((p+1)/2 * 256))</code>

:Also, your image will look smoother than the example you found on blogspot.com, since this imag is made with several octaves.

:--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 17:49, 5 March 2014 (UTC)

:: OK, I have added a post-condition and a comment in the opCall function in the D code. The image now looks smooth and nice, thank you. Do you also want to tell me what's the pre-condition of the "noise(double x, double y, double z)" Java method (this means, what's the allowed input range for those arguments x y z)? (Generally the more semantics you put in the code, then less likely is for people like me to use it wrongly). -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
::: x, y and z can be any real numbers.  Notice that the noise will be zero if they are all integers, though.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 18:19, 5 March 2014 (UTC)
::: I'm not 100% sure the result is guaranteed to be between -1 and 1.  It's an interpolation from the gradients on the grid points so sometimes it can exceed, but whether or not it can is not obvious.  So instead of putting a post-condition I think it's better to cap the output with noise = min(+1, max(-1, noise)); or something like that.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 18:24, 5 March 2014 (UTC)

==exactitudeness==

In this Rosetta Code task, it states:



''... show that the Perlin noise ... of a point in 3D-space with coordinates   3.14,   42,   7     is     0.13691995878400012.''


Is that number cast in stone, or is it a result of particular type of binary floating point arithmetic?

'''REXX''' uses decimal floating point (not binary) and the results are very close.

It doesn't matter how many "extra" decimal digits are specified in '''REXX''', the result is the same   (exactly 12 decimal digits):
::: 0.136919958784         and not
::: 0.13691995878400012 

So it would be impossible to show that this example (for the REXX entry), using decimal floating point, that the Perlin noise for that particular 3D point has that exact value.

Perhaps the wording could be tweaked a bit to reflect a value that (essentially) is approximated, depending on what form of floating point is being used:   IEEE 754 (binary format), IEEE-2008 (IBM hexadecimal format), Cray, decimal float, and/or a host of others.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:36, 7 September 2015 (UTC)


It would be interesting to see a   '''PL/I'''   entry that does both, binary floating point and decimal floating point.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:40, 7 September 2015 (UTC)


 
-----



: That result was achieved using numbers represented in the 64 bit IEEE-754 format. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:26, 7 September 2015 (UTC)

:: Yes, but the   ''exact''   value is   0.136919958784   when computed via (true) ''floating point decimal''   (with no rounding).   This should've been noted in the task's preamble.   For other 3D points, more decimal precision may be needed.   For the 3D point specified in this task, only 12 decimal digits are needed to calculate the exact result.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:47, 16 June 2016 (UTC)
