+++
title = "Talk:Brownian tree"
description = ""
date = 2019-01-14T01:24:37Z
aliases = []
[extra]
id = 7755
[taxonomies]
categories = []
tags = []
+++

== ''Collide'' isn't defined ==
Note that "collide" is not defined.  In one implementation "collide" means that the particle lands on a fixed particle (and then its previous position is used).  In another implementation (mine), "collide" means that the particle reaches a position adjacent to a fixed particle.  I do not know if this ambiguity matters, but I thought I would mention it.  --[[User:Rdm|Rdm]] 11:54, 14 July 2010 (UTC)
: It's interesting, but I don't think it matters; there are enough differences between languages that a retrace step in the algorithm doesn't seem important. --[[User:Short Circuit|Michael Mol]] 16:38, 14 July 2010 (UTC)
: It does make a difference in the resulting tree: the adjacency strategy makes particles have a larger "cross section" for collisions, and it will probably cause a difference in the appearance of the tree. But an even stronger influence is where the particles are added to the field. —[[User:Kevin Reid|Kevin Reid]] 22:25, 19 July 2010 (UTC)
:: Given a specific sequence of numbers to have fed out of the PRNG call (or using the same seed each time), it would be interesting to compare outputs from the two approaches. --[[User:Short Circuit|Michael Mol]] 15:53, 20 July 2010 (UTC)
:: The tree shape is very sensitive to how collision is defined.  The way each new particle attaching to the tree affects who itself can be used as an anchor later, sort of like a fractal.  From tests I did, small changes to some details (how far away the stuck particles are from each other, whether Brownian motion is slightly biased, at what distance are two particles considered colliding, etc) make the trees look completely different.  Some code examples produced images that don't look like trees at all, I guess this is why. --[[User:Ledrug|Ledrug]] 02:55, 11 June 2011 (UTC)

::: Yes, I also observed that minute changes can really effect the look of the brownian tree.  I was experimenting with the maximum field (with the REXX program) that I could view (in a DOS prompt screen under Windows) and tried various densities (expressed as a percentage), and it appeared that about 10% --&gt;25% ''looked'' about right (as far as esthetically looking brownian trees go).  To quote an old saw, ''I can't define what a brownian tree should like like, but I know one when I see one''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:28, 28 June 2012 (UTC)  

== C and D entries ==
Originally the D entry derived from the C entry. And later some other language entries have derived from the C/D entries. But the I think the C and C-derived entries don't respect the rules well (it moves already set particles, it doesn't always attach a particle if one of its Moore neighborhood cells is set), so I have rewritten the D entry. I think this D entry is closer to the literature value (1.71) of the resulting fractal dimension of DLA process. The fractal dimension is now lower than the C version (so the resulting tree is less dense), because the attach probability is higher, this causes a faster dendrite growth.--[[User:Bearophile|Bearophile]] 23:19, 27 June 2012 (UTC)

== REXX entry == 
After implementing and experimenting a while, I just thought about a restriction I placed on the movement of the particles (I called them dust motes).  

I was thinking in (more or less) of orthographical movements (with top being north); the particles could only move in intermediate (also called intercardinal) directions [N, NE, E, SE, S, SW, W, NW].  

I was using Moore's neighborhood as a basis for the particle movement.   

I was contemplating about going back and re-doing the particle movements, but that would probably make the randomization of the movements much more complex and that version of (a) Moore's neighborhood would be harder to compute (I should think). I'm not sure making the REXX program more complex (and bigger) would add anything to the this task, but it would make the movements more "lifelike". -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:28, 28 June 2012 (UTC)

== Example Images ==

Seeing that the link for the AutoHotkey image is dead (404), is it necessary to add an examle image as example output? Only 15 of the 35 languages have images embedded. --[[User:Oenone|Oenone]] ([[User talk:Oenone|talk]]) 14:28, 17 July 2014 (UTC)
