+++
title = "Category:Nc"
description = ""
date = 2011-07-31T20:17:44Z
aliases = []
[extra]
id = 10190
[taxonomies]
categories = []
tags = []
+++

{{library}}
'''nc''' is a shell command to make TCP or UDP connections. [[UNIX Shell]] scripts can use '''nc''' to connect to the internet.

'''nc''' is an important [[Unix]] tool, but '''nc''' is not one of the standard [[Unix]] commands. Some distros include '''nc''' in the base system. Other distros put '''nc''' in an optional ''netcat'' package. Scripts that use '''nc''' might not work until you install ''netcat''.

Netcat has at least four implementations:

* [Netcat 1.10](http://nc110.sourceforge.net/) is the original program by *Hobbit*.
* [GNU Netcat](http://netcat.sourceforge.net/) is by Giovanni Giacobbi.
* [OpenBSD nc(1)](https://www.openbsd.org/cgi-bin/man.cgi?query=nc&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html) (with [source code](https://www.openbsd.org/cgi-bin/cvsweb/src/usr.bin/nc/)) is a rewrite by Eric Jackson. It adds support for IPv6 and UNIX-domain sockets, and SOCKS and HTTPS proxies.
* [ncat](http://nmap.org/ncat/), from the Nmap Project, reimplements Netcat with several more features, like SCTP and SSL. The command name is '''ncat''', not '''nc'''.
