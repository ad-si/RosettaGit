+++
title = "Talk:Random number generator (included)"
description = ""
date = 2012-05-12T07:29:32Z
aliases = []
[extra]
id = 5376
[taxonomies]
categories = []
tags = []
+++

==Remove draft status?==
The entries so far are as I had hoped. --[[User:Paddy3118|Paddy3118]] 15:03, 24 January 2010 (UTC)

:A number of entries mention they invoke the rand() function from the C standard library.  The wikipedia page for the linear congruential generator (whatever) mentions that the rand() in the library of many popular C implementations is of this type.  There's also a link to a page that gives the spec. for the library's rand function.  The spec is pretty loose. It doesn't specify the algorithm to be used by the PRNG. Only the interface, and some other properties.  It does state that RAND_MAX must be 32767 or larger. 

:My question is - should there be a C entry that describes the situation with the rand() function of its standard library or not?

:: There should definitely be a C entry, because so many others defer to it, but, I thought that the C implementations were likely to vary on different platforms, (which, if true, should also be noted)? The (good), thing about C is that it is used everywhere, including in embedded systems, and many C environments only approach the spec of a standardization effort --[[User:Paddy3118|Paddy3118]] 06:19, 30 January 2010 (UTC)

== Main types of PRNG ==

This might be text sometime to transfer to the main task page.

:The main types of PRNG that are in use are the Linear Congruential Generator and the Generalized Feedback Shift Register (of which the mersenne twister generator is a subclass). The last main type is where the output of one of the previous ones (typically a mersenne twister) is fed through a cryptographic hash function to maximize unpredictability of individual bits.

:LCGs have the advantage of not requiring much state and being very fast to calculate, but produce random numbers with spectral problems. This makes them unsuitable for both Monte Carlo simulation and cryptography. By contrast, GFSRs (of which the Mersenne Twister is is particularly high quality version) require a lot more internal state and are considerably more expensive to compute and initialize (so much so that it is normal to use a LCG or simpler GFSR to drive the initialization); GFSRs tend to have much higher quality spectral properties than LCGs, and are suitable for use in Monte Carlo simulation. Neither LCGs nor GFSRs should be used for the most demanding applications (cryptography) without additional steps.

Probably needs more work. Feel free to massage! –[[User:Dkf|Donal Fellows]] 16:17, 24 January 2010 (UTC)
: Not much massaging done, just added some WP links. Ta! --[[User:Paddy3118|Paddy3118]] 04:06, 25 January 2010 (UTC)

== R- shows up under tasks not implemented ==

http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_R shows that this is not implemented in R, but it is.

How can I fix this?
: Seems you hit a caching issue. That page is updated on-demand, but only once every 15 minutes at most (Otherwise, the server wouldn't be able to handle the load). I just looked at it, and it looks fine. --[[User:Short Circuit|Michael Mol]] 14:45, 1 March 2010 (UTC)
: Some browsers cache things themselves for longer than others. Some corporate caches are pernicious swines when it comes to caches too. Oh well. (Hitting Shift-Reload sometimes works. Clearing the browser cache sometimes works. Giving it a rest and returning in 15 minute might work too.) –[[User:Dkf|Donal Fellows]] 14:55, 1 March 2010 (UTC)

== ALGOL 68 ==
The ALGOL 68 entry seems to state what properties a RNG should have, but doen't seem to state what algorithm is used or, conversely, if any RNG satisfying the constraints can/is used. --[[User:Paddy3118|Paddy3118]] 07:29, 12 May 2012 (UTC)
