+++
title = "Category talk:MUMPS"
description = ""
date = 2010-05-26T06:10:53Z
aliases = []
[extra]
id = 7423
[taxonomies]
categories = []
tags = []
+++

<p>I'm making the assumption that as a pedagogical site the examples should not be obfuscated. For example, from the http://rosettacode.org/wiki/A%2BB#MUMPS page, I would at work use:

```txt

R:20 Q:$T S W " ",$P(S," ")+$P(S," ",2) ;Wait 20 seconds for input, quit if nothing was input, write the numbers' sum

```

instead of the more legible

```txt

READ S
WRITE " ",$PIECE(S," ")+$PIECE(S," ",2)

```

Yes, I didn't do input checking on the example posted - it complicates the example, somewhat needlessly. If I should, I can go back and modify the examples.
--[[User:Stormneedle|Stormneedle]] 05:05, 26 May 2010 (UTC)

:The [[J|J language]] examples often go further, (thanks); and provide full explanations of some of their examples in talk pages as an ''extra''. It may not be necessary, but they sometimes do it (and I often read them - not being a J programmer but interested in how they get things done). --[[User:Paddy3118|Paddy3118]] 06:10, 26 May 2010 (UTC)
