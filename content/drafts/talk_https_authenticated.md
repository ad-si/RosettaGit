+++
title = "Talk:HTTPS/Authenticated"
description = ""
date = 2010-02-06T13:30:46Z
aliases = []
[extra]
id = 4144
[taxonomies]
categories = []
tags = []
+++

==What's it all about?==
What does “authentication” mean here anyway? ''(This was by me. —[[User:Dkf|Dkf]] 21:16, 9 May 2009 (UTC))''
: It looks like it's using standard HTTP authentication over an SSL tunnel.  HTTP natively supports auth without cookies or login forms.  It's not widely used. --[[User:Short Circuit|Short Circuit]] 22:13, 8 May 2009 (UTC)
:: If only it was not widely used. Our intranet is infested with it. Still, it's about the best we can do without webscraping (can't count on all login forms to work the same way...) Bad challenge really.  Or we could adjust the overall challenge to request demonstrating both this way and also via having the client present a certificate that the server understands (which means wrestling with the nastiness that is SSL configuration). —[[User:Dkf|Dkf]] 21:16, 9 May 2009 (UTC)
::: Then, really, the challenge should be broken into four distinct parts.  HTTP connection, HTTPS connection, HTTP auth and cert auth as an SSL client. --[[User:Short Circuit|Short Circuit]] 03:23, 10 May 2009 (UTC)
:::: Added [[HTTPS Request]] as part of that plan. —[[User:Dkf|Donal Fellows]] 10:12, 1 June 2009 (UTC)
:::: Adding [[Client-Authenticated HTTPS Request]] as part of that plan. —[[User:Dkf|Donal Fellows]] 10:19, 1 June 2009 (UTC)
