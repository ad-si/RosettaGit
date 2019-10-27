+++
title = "Public Domain Korn Shell"
description = ""
date = 2011-09-13T00:27:02Z
aliases = []
[extra]
id = 4795
[taxonomies]
categories = []
tags = []
+++

{{implementation|UNIX Shell}}

pdksh is the ''public domain Korn shell'', a clone of the [[Korn Shell]]. It has most of the ksh88 features, and almost none of the ksh93 features.

The last version, [http://web.cs.mun.ca/~michael/pdksh/ pdksh 5.2.14] from 1999 July 13, still has several bugs. Systems like [http://packages.debian.org/sid/pdksh Debian], [http://www.openbsd.org/cgi-bin/cvsweb/src/bin/ksh/ OpenBSD] and [http://cvs.pld-linux.org/cgi-bin/cvsweb.cgi/packages/pdksh/ PLD] now apply several patches to pdksh.

* If an example from Rosetta Code "works with pdksh", then someone might have tested the example with one of these patched versions of pdksh.
* If a <tt>#!/bin/sh</tt> script "works with pdksh", then the tester might have used [[OpenBSD]], where both <tt>/bin/ksh</tt> and <tt>/bin/sh</tt> are pdksh.

[[mksh]] is a successor to pdksh. mksh consolidates several patches and a few new features into one shell. If you want to install pdksh, then you might instead install [[mksh]].
