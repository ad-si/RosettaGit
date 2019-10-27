+++
title = "Talk:Bitmap/Flood fill"
description = ""
date = 2014-09-23T19:39:03Z
aliases = []
[extra]
id = 3563
[taxonomies]
categories = []
tags = []
+++

What about ppm image? Since we already have [[read ppm file]] task. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:13, 18 February 2009 (UTC)
: Ops, yes, I've used [[Read image file through a pipe]], which has just 1 C implementation (mine:D). Now I am leaving, but tonight I will upload the ppm converted image, if no one else already did that. --[[User:ShinTakezou|ShinTakezou]] 17:51, 18 February 2009 (UTC)
: Back to my steps: I can't upload PPM files; tried cheating with a different ext, but it says "corrupted"... so... it is up to you to convert the jpg into PPM if you need it... ;) --[[User:ShinTakezou|ShinTakezou]] 22:27, 18 February 2009 (UTC)
: In case someone can't convert it, you can download a 50% smaller PPM version [http://www.capo-nord.org/soci/xmav/filepot/RosettaCode/misc/Unfilledcirc.ppm here] (until the site will be up :D)--[[User:ShinTakezou|ShinTakezou]] 00:59, 19 February 2009 (UTC)

How about a 3d flood fill?

the scipy subpackage ndimage has a multidimensional implementation for reference --[[User:Tinku99|Tinku99]] 01:26, 26 November 2009 (UTC)
: Interesting, but I think we'd need a common volumetric target format, similar to [[Basic bitmap storage]], but in three (or, more interestingly, N.) dimensions.  I'm not saying that's hard, but it's something of a prerequisite. ( a subsequent derivation of the image format to support M channels per pixel would be even more interesting.) --[[User:Short Circuit|Michael Mol]] 08:38, 26 November 2009 (UTC)

IMO the easiest thing would be to use netpbm pgm and ppm formats and simply allow more than 2 dimensions on the line where we normally puts rows and columns.  alternatively we could have a simple binary format where the first line would be the equivalent declaration in c followed by a newline character and then the binary data of the image / matrix [[User:Tinku99|Tinku99]] 07:21, 29 November 2009 (UTC)
: Make it another task though. 2D Flood Filling is a reasonable task in itself and total generalization to arbitrary dimensions is something for mathematicians. (I don't think anyone uses flood filling algorithms in 3D these days anyway.) –[[User:Dkf|Donal Fellows]] 18:59, 29 November 2009 (UTC)

== The meaning of. ==

About the notice «Very difficult to make it work, and still doesn't work correctly after that». 

What does it mean exactly "difficult to make it work"? The user found it difficult to compile it? (I.e., s/he failed compiling it)? And when s/he succeeded, s/he found it do not work correctly? 

As it is (or as it was), it works, provided that: a) you compile it as BSD, because it uses sys/queue.h e.g. with gcc use -D_BSD_SOURCE as compiling option; b) you link it to other image tasks, from which it depends upon and that I've collected once into a single "imglib" — a "package" is available as explained at [[User:ShinTakezou]].

Not very straightforward for the reader who wants hands on as quickly as possible, especially since I've not provided compiling instructions, so I agree on the first part. '''But''', about the second part ("still doesn't work correctly") it would be appreciated some insight. Despite an apparent initial "noise", the algorithm itself is clear and "squeezed" into something like 20 lines of code. [[User:ShinTakezou|ShinTakezou]] ([[User talk:ShinTakezou|talk]]) 19:39, 23 September 2014 (UTC)
