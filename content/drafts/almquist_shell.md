+++
title = "Almquist Shell"
description = ""
date = 2014-11-09T15:11:56Z
aliases = []
[extra]
id = 4796
[taxonomies]
categories = []
tags = []
+++

{{implementation|UNIX Shell}}
'''Almquist Shell''' is a minimal implementation of a [[POSIX]] shell, and also a replacement for [[compatible with::Bourne Shell]]. Almquist Shell has more features than Bourne Shell, but fewer features than most other shells. (No arrays!) Almquist Shell only implements POSIX features, plus a few [[BSD]] traditions, like its <code>local</code> command. Almquist Shell is the default shell, <code>/bin/sh</code>, of some systems.

If a script works with Almquist Shell, it will probably also work with [[bash]], [[pdksh]] and [[zsh]]. Further, it will probably work with [[ksh93]] ''unless'' it uses <code>local</code>, which ksh93 lacks.

Almquist Shell filled the need for a free shell to replace Bourne Shell. Kenneth Almquist posted [http://groups.google.com/group/comp.sources.unix/msg/2774e7653a8e6274 the first version of Ash] to Usenet group comp.sources.unix at 30 May 1989. It was a clone of SVR3 Bourne Shell. [[BSD]] used Ash for <code>/bin/sh</code>, added features from POSIX, and put a Berkeley copyright on this shell.

== Almquist variants ==
Ash has three major variants:

* [[Debian Almquist Shell]] (Dash)
* [http://svnweb.freebsd.org/base/head/bin/sh/ FreeBSD /bin/sh]
* [http://cvsweb.netbsd.org/bsdweb.cgi/src/bin/sh/?only_with_tag=MAIN NetBSD /bin/sh]

All three variants have similar features. Dash can run on GNU/Linux.

Ash is also the shell provided by [[BusyBox]].

== See also ==
* [[wp:Almquist shell|Almquist shell]], Wikipedia's article
* [http://www.in-ulm.de/~mascheck/various/ash/ Ash (Almquist Shell) Variants], a history of many Ash versions and their features
