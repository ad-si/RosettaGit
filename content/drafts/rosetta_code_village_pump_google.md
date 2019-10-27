+++
title = "Rosetta Code:Village Pump/Google"
description = ""
date = 2009-10-05T14:52:12Z
aliases = []
[extra]
id = 4865
[taxonomies]
categories = []
tags = []
+++

It looks like Google is returning '''weird''' results. I typed in "powerbasic" (because I'm too lazy to do it right) and, while the first result is correct ([[PowerBASIC]]), the second is decidedly ''not'' correct: instead of PowerBASIC/Omit, it links to villagepump.rosettacode.org/wiki/Category:PowerBASIC/Omit, which is (of course) 404. (Actually, many of the results are to villagepump.rosettacode.org.) Something just ain't right... or is this expected behavior? -- [[User:Eriksiers|Eriksiers]] 00:14, 3 October 2009 (UTC)
: To be clear, you were using Google?  I tried a [http://www.google.com/search?q=powerbasic+site%3Arosettacode.org simple search], and I saw what you're referring to.  It looks to me like Google didn't stop crawling links to villagepump.rosettacode.org, long after that subdomain was dropped. (It used to whole an SMF forum install.)  My best guess is that those "pages" were indexed between the time the subdomain was dropped and when I set up the redirect of (x).rosettacode.org/y to rosettacode.org/x/y.  It's possible the DNS components weren't dropped (or were cached), while the virtualhost directives on the server no longer trapped the subdomain.  If they were trapped by the default domain, and the default vhost served up rosettacode.org/ content, then I could see how those pages would get indexed that way.  It doesn't really make sense why GoogleBot wouldn't have figured it out; I'm serving up the correct headers for the error code. And I'm using 301 permanent redirects for the redirect from the subdomain. Might be time to try setting up the sitemap again. --[[User:Short Circuit|Michael Mol]] 05:22, 3 October 2009 (UTC)
::I started the search by using the "search" box to the left, then continuing on from the "search results" (a.k.a. "searching is disabled") page. The resulting URL is [http://www.google.com/search?domains=http%3A%2F%2Frosettacode.org&num=50&ie=utf-8&oe=utf-8&q=powerbasic&btnG=Search&sitesearch=http%3A%2F%2Frosettacode.org long and complex.] Unfortunately, it looks like the redirects aren't working; every link just gives 404.
::How long ago did you change the layout? It might help to send Google an email. -- [[User:Eriksiers|Eriksiers]] 14:52, 5 October 2009 (UTC)
