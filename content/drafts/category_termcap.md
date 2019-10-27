+++
title = "Category:Termcap"
description = ""
date = 2012-03-10T01:12:59Z
aliases = []
[extra]
id = 11486
[taxonomies]
categories = []
tags = []
+++

{{library}}
This category groups programs using the obsolete ''termcap'' library.

Early [[Unix]] systems used ''termcap'' to send escape sequences for [[terminal control]]. The ''termcap'' database described several types of terminals. The ''termcap'' library queried this database.

'''termcap is obsolete.''' Most systems switched to ''terminfo'' during 1980s or 1990s. All recent systems have ''terminfo'', except [[NetBSD]] (but [http://blog.netbsd.org/tnf/entry/terminfo_has_replaced_termcap NetBSD 6 will have terminfo]). Some NetBSD users installed [[:Category:ncurses|''ncurses'']] to get ''terminfo''. Therefore, the only reason to use ''termcap'' is to remain compatible with ancient Unix systems, or with NetBSD systems without ''ncurses''.

== Manuals ==
* [http://invisible-island.net/ncurses/man/curs_termcap.3x.html curs_termcap(3x)], manual page from [[:Category:ncurses|''ncurses'']]
* [http://www.openbsd.org/cgi-bin/man.cgi?query=termcap&apropos=0&sektion=5&manpath=OpenBSD+Current&arch=i386&format=html termcap(5)], manual page from [[OpenBSD]]
