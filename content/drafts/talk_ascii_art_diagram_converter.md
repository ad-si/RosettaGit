+++
title = "Talk:ASCII art diagram converter"
description = ""
date = 2014-04-02T03:43:17Z
aliases = []
[extra]
id = 17146
[taxonomies]
categories = []
tags = []
+++

== D solution won't compile ==

I installed the Digital Mars D compiler version 2.064.2 on my Linux box (x86_64 architecture), copied the snippet into a file, and passed it to dmd - but it failed with this error:

```txt

$ dmd ascii_struct.d
ascii_struct.d(121): Error: cannot implicitly convert expression ((line.length - 1LU) / 3LU) of type ulong to uint
ascii_struct.d(139): Error: cannot implicitly convert expression ((field.length + 1LU) / 3LU) of type ulong to uint
ascii_struct.d(188): Error: CTFE failed because of previous errors in makeStructFromDiagram

```

Are you sure that the code is correct?

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 23:09, 30 January 2014 (UTC)



The code there only worked on 32 bit, but it is fairly easy to fix. I'll do it now.
