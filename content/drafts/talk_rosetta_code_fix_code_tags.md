+++
title = "Talk:Rosetta Code/Fix code tags"
description = ""
date = 2012-12-06T18:09:31Z
aliases = []
[extra]
id = 3321
[taxonomies]
categories = []
tags = []
+++

I replaced GeshiCodeTag's behavior with respect to unsupported languages so that it just falls back uses an HTML &lt;code&gt; block.  These will be distinguishable from GeSHi-targeted &lt;code&gt; blocks because they won't have a language parameter associated with them.  Be mindful of that. --[[User:Short Circuit|Short Circuit]] 08:01, 30 January 2009 (UTC)

<nowiki>I already solved this bug.
It's a very poor solution, it finds "<code>" and "</code>" indexes, replace them to "c0de", so re.sub() will ignore it.
At script end "c0de" will be replaced to "code" again.
Now the script can be 100% automatated, using Wikimedia API. (I think.)</nowiki> --[[User:Guga360|Guga360]] 16:28, 30 January 2009 (UTC)

== Please no "code" to "tt" change! ==
I dislike the new change of "fixing" <nowiki><code>...</code> to <tt>...</tt></nowiki>. The disabling of code tags is temporary, and using code tags (logigal markup) instead of tt tags (physical markup) is the Right Thing. I ''intentionally'' did not change code tags meant as inline code tags when changing code->lang (I did change inline code tags back when it seemed that the new meaning of code would prevail). --

: Sorry it was me; I realized your point and stopped me by myself before reading this, nonetheless I've already done little damages and will go fixing them as soon as I can. --[[User:ShinTakezou|ShinTakezou]] 15:46, 2 February 2009 (UTC)

== Self-modifying code==

I believe CodeTagFixer is now self-modifying.  (Insert obligatory Skynet reference :-D )--[[User:Short Circuit|Short Circuit]] 21:14, 3 February 2009 (UTC)
:It's possible to protect this article from begin edited by bots? --[[User:Guga360]]
::Kinda sorta, yeah.  I can currently offer varying levels of protection.  Allow all normal access, disallow anonymous or disallow anyone but sysops.  So I'd have to make it a sysops-only page, which stymies the page.  It'd be better if your bot could be modified to attempt to detect code snippet boundaries and not modify content within them. --[[User:Short Circuit|Short Circuit]] 23:35, 3 February 2009 (UTC)

== ambiguity ==

This task is inadequately defined, since any tag would match <%s>.

== REXX ==

should old.j be old.k ???

: yuppers, fixed. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:09, 6 December 2012 (UTC)
