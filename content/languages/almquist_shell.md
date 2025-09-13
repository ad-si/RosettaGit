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
'''Almquist Shell''' is a minimal implementation of a [POSIX](https://rosettacode.org/wiki/POSIX) shell, and also a replacement for [compatible with::Bourne Shell](https://rosettacode.org/wiki/compatible_with::Bourne_Shell). Almquist Shell has more features than Bourne Shell, but fewer features than most other shells. (No arrays!) Almquist Shell only implements POSIX features, plus a few [BSD](https://rosettacode.org/wiki/BSD) traditions, like its <code>local</code> command. Almquist Shell is the default shell, <code>/bin/sh</code>, of some systems.

If a script works with Almquist Shell, it will probably also work with [bash](https://rosettacode.org/wiki/bash), [pdksh](https://rosettacode.org/wiki/pdksh) and [zsh](https://rosettacode.org/wiki/zsh). Further, it will probably work with [ksh93](https://rosettacode.org/wiki/ksh93) ''unless'' it uses <code>local</code>, which ksh93 lacks.

Almquist Shell filled the need for a free shell to replace Bourne Shell. Kenneth Almquist posted [http://groups.google.com/group/comp.sources.unix/msg/2774e7653a8e6274 the first version of Ash] to Usenet group comp.sources.unix at 30 May 1989. It was a clone of SVR3 Bourne Shell. [BSD](https://rosettacode.org/wiki/BSD) used Ash for <code>/bin/sh</code>, added features from POSIX, and put a Berkeley copyright on this shell.

## Almquist variants
Ash has three major variants:

* [Debian Almquist Shell](https://rosettacode.org/wiki/Debian_Almquist_Shell) (Dash)
* [http://svnweb.freebsd.org/base/head/bin/sh/ FreeBSD /bin/sh]
* [http://cvsweb.netbsd.org/bsdweb.cgi/src/bin/sh/?only_with_tag=MAIN NetBSD /bin/sh]

All three variants have similar features. Dash can run on GNU/Linux.

Ash is also the shell provided by [BusyBox](https://rosettacode.org/wiki/BusyBox).

## See also
* [Almquist shell](https://en.wikipedia.org/wiki/Almquist_shell), Wikipedia's article
* [http://www.in-ulm.de/~mascheck/various/ash/ Ash (Almquist Shell) Variants], a history of many Ash versions and their features
