+++
title = "Rosetta Code:Village Pump/MediaWiki 1.20 Upgrade Issues"
description = ""
date = 2013-04-24T08:40:42Z
aliases = []
[extra]
id = 13231
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=MediaWiki 1.20 Upgrade Issues
|summary=Report here any issues observed after the upgrade to MediaWiki 1.20.
}}
==Languages Not Implemented in X Pages Not Working==

HTTP 500 errors arise when visiting, e.g. [[Reports:Tasks not implemented in Tcl]]. (h.t. [[User:Dkf|Dkf]]) --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)


### Details


MultiCategory Search extension is likely broken in MW 1.20. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)


### Solutions


* Try to replace with Semantic MediaWiki queries. (Anyone can give this a shot...leave details of attempts here.) --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)
* If replacement with SMW queries doesn't work, I'll dig into the error logs this evening and see about fixing the bug. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)

* Upgraded MultiCategorySearch. At this point, upstream contains all of the features we'd been maintaining locally. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 03:55, 2 April 2013 (UTC)

==Account Creation Not Fully Functional==

Some attempts to create accounts (even by me, while logged in as an administrator account) fail with "Login error Incorrect or missing confirmation code http://rosettacode.org create account". Clearly, this doesn't happen to everybody; new accounts are created every day. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)


### Details


This predates the upgrade to MW 1.20, and looks more likely to be related to the CloudFlare deployment. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)


### Solutions


* I need to check the firewall logs to see if I'm blocking interactions between RC's server and ReCaptcha. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)
* It might also be a caching issue, if ReCaptcha's servers are making a query against RC's server (via CloudFlare), and getting a cached result back. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:45, 1 April 2013 (UTC)

==Miscellaneous server issues==

The server was OOMing. MaxSpareServers was too high. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 13:51, 1 April 2013 (UTC)

PHP-APC was utterly failing to allocate memory pools. Uninstalled. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 03:56, 2 April 2013 (UTC)


### Debian iceweasel cannot open php file


There is a problem on my home computer following upgrade, but I get an error to save or choose application to open "PHP" file when I attempt to edit. I read that this is due to Mime type not being given as Text/HTML on the server side. Saving the files gives zero length (empty) files on the Mediawiki upgrade. I'm not sure whether anyone else is seeing this. I am using old version of Debian. [[User:Markhobley|Markhobley]] ([[User talk:Markhobley|talk]]) 19:21, 15 April 2013 (UTC)

==Title image missing==
Minor issue: The Rosetta Code title image (which is apparently supposed to be at http://rosettacode.org/mw/title.png) appears to be missing, leaving a blank in the corner of the page when using either the Vector or MonoBook styles. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:40, 24 April 2013 (UTC)
