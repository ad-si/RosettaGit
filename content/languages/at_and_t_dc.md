+++
title = "AT&T dc"
description = ""
date = 2011-12-12T22:53:30Z
aliases = []
[extra]
id = 10394
[taxonomies]
categories = []
tags = []
+++
The original ''dc'' interpreter appeared in Version 1 (V1) AT&T UNIX. Therefore, ''dc'' precedes the [C language](https://rosettacode.org/wiki/:Category:C) (appeared in V3 UNIX) and the [Bourne Shell](https://rosettacode.org/wiki/Bourne_Shell) (appeared in V7 UNIX). The authors programmed the interpreter in [PDP-11 Assembly](https://rosettacode.org/wiki/:Category:PDP-11_Assembly), and later rewrote it in C language. AT&T dc became the back end of [AT&T bc](https://rosettacode.org/wiki/AT&T_bc) in V6 UNIX. AT&T dc survives in systems that descend from System V, including [Solaris](https://rosettacode.org/wiki/Solaris).

POSIX only describes [''bc''](https://rosettacode.org/wiki/:Category:bc), not ''dc''; so AT&T dc is the only reference. Other implementations (like [GNU dc](https://rosettacode.org/wiki/GNU_dc) and [OpenBSD dc](https://rosettacode.org/wiki/OpenBSD_dc)) follow AT&T dc by implementing the same commands.

The manual page accidentally omits <code>!< != !></code>, but AT&T dc does have these commands, and ''dc'' programmers do use them.

## Ancient dc
[http://www.tuhs.org/wiki/The_Unix_Heritage_Society The Unix Heritage Society] preserves old versions of AT&T UNIX.

* [http://minnie.tuhs.org/cgi-bin/utree.pl?file=V1/man/man1/dc.1 dc(I) manual], Version 1 AT&T UNIX
* [http://minnie.tuhs.org/cgi-bin/utree.pl?file=V2/cmd source code] (dc1.s to dc5.s), Version 2 AT&T UNIX

## Modern dc
The [http://heirloom.sourceforge.net/ Heirloom Project] provides AT&T dc along with other System V commands.
* [http://heirloom.sourceforge.net/man/dc.1.html dc(1) manual] and [http://heirloom.cvs.sourceforge.net/viewvc/heirloom/heirloom/dc/ source code]
