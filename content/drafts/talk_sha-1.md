+++
title = "Talk:SHA-1"
description = ""
date = 2012-01-19T21:26:18Z
aliases = []
[extra]
id = 11227
[taxonomies]
categories = []
tags = []
+++

== Implementations separated ==

We might want to set this up like [[MD5]] and [[MD5/Implementation]] just to keep things consistent. --[[User:Mwn3d|Mwn3d]] 21:40, 18 January 2012 (UTC)

: Some implementations of MD5 are still on [[MD5]] and not on [[MD5/Implementation]]. If someone creates SHA-1/Implementation, please move the Ruby implementation from here to there. If I later create RIPEMD-160, SHA-256, or so on, I might not immediately create /Implementation pages. --[[User:Kernigh|Kernigh]] 19:23, 19 January 2012 (UTC)

== Warning highlighting ==

That highlighted warning should probably go into a centered infobox to make it more visible. –[[User:Dkf|Donal Fellows]] 10:45, 19 January 2012 (UTC)

: What kind of warning is appropriate for SHA-1? [http://docs.factorcode.org/ Factor's documentation] for [http://docs.factorcode.org/content/article-checksums.sha.html SHA] says, "SHA-1 is considered insecure, while SHA-2 It is generally considered to be pretty strong." OpenBSD [http://www.openbsd.org/cgi-bin/man.cgi?query=sha1&apropos=0&sektion=1&manpath=OpenBSD+5.0&arch=i386&format=html sha(1)] gives no warning at all, though OpenBSD [http://www.openbsd.org/cgi-bin/man.cgi?query=md5&apropos=0&sektion=1&manpath=OpenBSD+5.0&arch=i386&format=html md5(1)] gives a warning and recommends sha256. --[[User:Kernigh|Kernigh]] 19:23, 19 January 2012 (UTC)
:: I don't think the issue is with the content of the warning. The issue is with how it looks. --[[User:Mwn3d|Mwn3d]] 20:21, 19 January 2012 (UTC)
::: Exactly that. –[[User:Dkf|Donal Fellows]] 20:23, 19 January 2012 (UTC)
:I just moved the warning and put it in a box. How's that look? --[[User:Mwn3d|Mwn3d]] 20:31, 19 January 2012 (UTC)
:: Fine. It draws the eye in; it's the sort of thing needed. (We probably ought to make sure that [[MD5]] is similarly marked; I'll check that in a few seconds.) –[[User:Dkf|Donal Fellows]] 21:07, 19 January 2012 (UTC)
::: Both now follow the same pattern. Could do with a link to the weaknesses in MD5, but that's not a high priority as it is well known to be be superseded. –[[User:Dkf|Donal Fellows]] 21:26, 19 January 2012 (UTC)
