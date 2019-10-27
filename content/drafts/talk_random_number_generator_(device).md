+++
title = "Talk:Random number generator (device)"
description = ""
date = 2011-05-08T04:47:25Z
aliases = []
[extra]
id = 9144
[taxonomies]
categories = []
tags = []
+++

==Primer notes==
On Linux, reading from /dev/urandom pulls from the system entropy pool (which may contributed to by HRNGs, among other sources), but provides psuedorandom numbers if that entropy pool dries up. reading from /dev/random, on the other hand, pulls from the same entropy pool, but blocks whenever the entropy pool dries up. --[[User:Short Circuit|Michael Mol]] 14:03, 11 January 2011 (UTC)

: I guess that a solution may use either /dev/random or /dev/urandom. The entropy pool probably has enough entropy for a 32-bit integer. I prefer to use /dev/urandom. Many kernels use a secure hash, like SHA1, to convert the entropy pool to random numbers. ([http://cvsweb.netbsd.org/bsdweb.cgi/src/sys/dev/rndpool.c?rev=1.20&content-type=text/x-cvsweb-markup&only_with_tag=MAIN NetBSD uses SHA1.]) Even if the entropy pool is dry, the random numbers should remain secure. I guess that you would have to reverse or defeat SHA1 to replicate the pseudorandom numbers from a dry pool; but SHA1 is a secure hash so difficult to reverse.

: OpenBSD 4.8 and before had /dev/urandom giving random numbers, but /dev/random giving only errors. (This was because /dev/random was a reserved device, and /dev/srandom of OpenBSD was like /dev/random of Linux.) So /dev/urandom was better than /dev/random for OpenBSD. This has changed with OpenBSD 4.9 (from May 2011); now /dev/random also gives random numbers. So if some example on Rosetta Code uses /dev/random, I will not care to change it to /dev/urandom. --[[User:Kernigh|Kernigh]] 04:47, 8 May 2011 (UTC)

==Libraries and features==
Is it within the scope of this task to use libraries and language features which provide random numbers derived from both hardware and software sources, or is the intent here to show a more low-level access to the underlying operating system itself? --[[User:Short Circuit|Michael Mol]] 14:03, 11 January 2011 (UTC)
:I would say yes, if there is some hardware involved, so that the random numbers are not 100% "pseudo". The stress should be on random numbers. Just showing how to read from an arbitrary device is perhaps not very interesting. --[[User:Abu|Abu]] 14:25, 11 January 2011 (UTC)
:: To be certain, the task's fundamental drive is to obtain numbers which are ''not'' deterministically-generated, even with an unknown seed? --[[User:Short Circuit|Michael Mol]] 14:44, 11 January 2011 (UTC)
::: Er. I was unclear. Clarification handy, though: [http://irclog.perlgeek.de/rosettacode/2011-01-11#i_3175701 http://irclog.perlgeek.de/rosettacode/2011-01-11#i_3175701] --[[User:Short Circuit|Michael Mol]] 15:21, 11 January 2011 (UTC)
:::: OK: So let's say, it should involve real-world generated entropy during each step, and define the scope of this task to what is covered by the Wikipedia article. --[[User:Abu|Abu]] 15:26, 11 January 2011 (UTC)
