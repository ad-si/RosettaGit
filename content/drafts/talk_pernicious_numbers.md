+++
title = "Talk:Pernicious numbers"
description = ""
date = 2014-03-12T20:16:28Z
aliases = []
[extra]
id = 17365
[taxonomies]
categories = []
tags = []
+++

==C solution too cryptic?==
Hi, the C solution is a bit mysterious to me.  The for loop seems to "remove the zeros" if I understand correctly, but then what's so special about 2693408940 ??  How can a simple bitwise & tell if the number of 1 (which is now the length since all zeros have been removed) is prime?
A bit of explanation in the introduction of the section or in comment would be welcome--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 14:43, 12 March 2014 (UTC)
:Edit, hum after some analysis it appears that 2693408940 encodes the prime numbers in its binary form:  10100000100010100010100010101100.   From right to left, starting from 0, the one marks the primes.  Sill this could use some explanations.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 14:51, 12 March 2014 (UTC)
:OK, I understand now.  I added some comments in the code.  Hope it's ok.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:02, 12 March 2014 (UTC)
:: Er yeah, it was pretty cryptic, and your analysis is absolutely correct.  But I changed the routine again for something faster, though I did put in some comments this time.  I didn't retain the "0b010011..." literal format since it's a GCC exteinsion, but if you want it back in I'm not really troubled by it. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:40, 12 March 2014 (UTC)

==Wiki link removal==

Why was the Wikipedia link removed (for Pernicious number) ? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:00, 12 March 2014 (UTC)

The "hidden link" to Wiki's link within the definition of ''pernicious number''  isn't obvious unless one hovers over it and then Rosetta Code then underlines/underscores it, indicating that it's a link (either that, or my eyes are getting really bad).   I can't see the harm on having an explicit link listed with the others. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:16, 12 March 2014 (UTC)
