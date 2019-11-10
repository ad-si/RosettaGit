+++
title = "Talk:Execute Brainfuck/TI-83 BASIC"
description = ""
date = 2010-02-06T14:26:16Z
aliases = []
[extra]
id = 5003
[taxonomies]
categories = []
tags = []
+++

At least on my calculator (TI-83 Plus), this returns a bunch of errors - mostly domain errors from sub().
[[User:BR|BR]] 19:53, 30 November 2009 (UTC)
:I copied it from my calculator (also a TI-83 plus) and it works here. I could have copied things wrong though. I'll go over it again sometime soon. --[[User:Mwn3d|Mwn3d]] 20:28, 30 November 2009 (UTC)
:I just fixed a few things. See if it works with those differences. It was all string variable numbering problems. If it were up to me I'd give them all useful names, but TI-BASIC doesn't have that for the 83. --[[User:Mwn3d|Mwn3d]] 20:37, 30 November 2009 (UTC)
==Str5?==
There is <code>" "→Str5</code> but Str5 is not reused. Can this be deleted, or does this indicate some logic mistake? --[[User:Kevin Reid|Kevin Reid]] 17:02, 1 December 2009 (UTC)
:I think it was a typo from when it was being changed around a bit before. It's gone now. --[[User:Mwn3d|Mwn3d]] 02:17, 6 January 2010 (UTC)
==Improvements==
The TI83+ supports setting a list to a given size by doing <code>999→dim(L1</code>. But keep in mind each element takes up 9 bytes of RAM.

Also the loop handling might be improved by having L1 be the memory and L2 being the return address stack. That way you only have to check the brackets when ] or EOF is encountered.

--[[User:BenBE|BenBE]] 00:55, 6 January 2010 (UTC)
