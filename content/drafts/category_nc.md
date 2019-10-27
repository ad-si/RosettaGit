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

* [http://nc110.sourceforge.net/ Netcat 1.10] is the original program by *Hobbit*.
* [http://netcat.sourceforge.net/ GNU Netcat] is by Giovanni Giacobbi.
* [http://www.openbsd.org/cgi-bin/man.cgi?query=nc&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html OpenBSD nc(1)] (with [http://www.openbsd.org/cgi-bin/cvsweb/src/usr.bin/nc/ source code]) is a rewrite by Eric Jackson. It adds support for IPv6 and UNIX-domain sockets, and SOCKS and HTTPS proxies.
* [http://nmap.org/ncat/ ncat], from the Nmap Project, reimplements Netcat with several more features, like SCTP and SSL. The command name is '''ncat''', not '''nc'''.
