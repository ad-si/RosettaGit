+++
title = "Rosetta Code:Village Pump/Enable Mediawiki API Edit"
description = ""
date = 2010-11-10T01:49:48Z
aliases = []
[extra]
id = 3339
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Enable Mediawiki API Edit
|summary=Request enabling of the edit API
}}
I'm trying to make bot that automagically applys [[Code Tag Fixer]] in all Tasks using Mediawiki API (http://www.rosettacode.org/w/api.php), but Mediawiki API "action=edit" is disabled, so i can't edit pages automagically with Mediawiki API, making a bot impossible.

Can some admin enable this function? --[[User:Guga360|Guga360]] 18:09, 2 February 2009 (UTC)

PS: Ignore my poor english.
: Because of the potential for abuse, I'm going to need each bot to have its own authentication credentials, and I'll grand API privileges on a per-bot basis.  Create an account for your bot and give me the details. I'll work on it when I get home.--[[User:Short Circuit|Short Circuit]] 19:26, 2 February 2009 (UTC)
::OK, bot created, i'm waiting to be put in Bots group. --[[User:GugaTagFixer|GugaTagFixer]] 19:49, 2 February 2009 (UTC)
::My bot is almost 98% complete, I create a lot of functions. But now, the function is
::
::
```python
apply("Article_Title",get_token("MyBot","MyPassword")
```

::
::I need just iterate apply() in allpages. --[[User:Guga360|Guga360]] 21:43, 2 February 2009 (UTC)


While we're on the subject, does anyone think we should have [[Help:Adding a Bot]]? Does it need to be that formal? Will we have that many bots? --[[User:Mwn3d|Mwn3d]] 20:24, 2 February 2009 (UTC)
:No, Wikipedia already has a "Creating a bot" article, it's the same thing. A [[Help:Adding a Bot]] can link to a Wikipedia article. Or something else.--[[User:Guga360|Guga360]] 21:43, 2 February 2009 (UTC)
: I expect we'll have as many bots as are well-behaved, do useful things, and don't completely suck up my slice's CPU and bandwidth allocation.--[[User:Short Circuit|Short Circuit]] 23:41, 2 February 2009 (UTC)

My bot is 100% complete, i'm waiting my [[User:GugaTagFixer|bot]] to be put in [[Bots]] group. --[[User:GugaTagFixer|GugaTagFixer]] 00:44, 3 February 2009 (UTC)
: I'll add it in a moment.  In the mean time, would you mind posting the entire code of your bot (sans its actual login name and password) in its user page, so other folks can review and comment? --[[User:Short Circuit|Short Circuit]] 01:43, 3 February 2009 (UTC)

:: It isn't working. "Unrecognized value for parameter 'action': edit", What happened? And yes, I'm logged in, because i can list 5000 pages perfectly. Did you enable $wgEnableWriteAPI in LocalSettings.php? --[[User:Guga360|Guga360]] 02:38, 3 February 2009 (UTC)
::: It's set to true now. --[[User:Short Circuit|Short Circuit]] 03:55, 3 February 2009 (UTC)
::: Try again.  I've now explicitly set both wgEnableAPI and wgEnableWriteAPI, and added a few privileges to the bot group.  --[[User:Short Circuit|Short Circuit]] 08:39, 3 February 2009 (UTC)
:::: Now it's working perfectly, i already solved "article with _" bug.
:::::It;s not perfect. It's messing up redirects. --[[User:Mwn3d|Mwn3d]] 15:38, 3 February 2009 (UTC)
::::::I already fixed it. But this is a problem in Wikimedia API, not in my code.
:::::::We also need to go back through the Ada examples and change <nowiki>"
```
" to "
```
"</nowiki>--[[User:Mwn3d|Mwn3d]] 15:57, 3 February 2009 (UTC)
::::::::I will fix it.
:::::::::Fixed, running bot again.
::::::::::I'm using a better [[Code Tag Fixer]], but it does not fix Solutions with '''<nowiki>
```txt
</nowiki>''', it's possible replace: '''<nowiki>{{header|Modula-3}}\n
```txt
.*
```
</nowiki>''' with '''<nowiki>{{header|Modula-3}}\n<lang modula-3>.*
```
</nowiki>'''? I think that this will be pretty hard, but possible. (And buggy.) --[[User:GugaTagFixer|GugaTagFixer]] 16:10, 3 February 2009 (UTC)

[[User:Mwn3d|Mwn3d]]: My bot fixes deprecated tags, not incorrect tags. --[[User:GugaTagFixer|GugaTagFixer]] 16:13, 3 February 2009 (UTC)
:Sorry, I didn't mean that your bot needed to do it. I just meant it needed to be done. --[[User:Mwn3d|Mwn3d]] 16:35, 3 February 2009 (UTC)
::OK, i will try to making another bot, to fix incorrect tags. It almost done. --[[User:GugaTagFixer|GugaTagFixer]] 17:08, 3 February 2009 (UTC)

Anyone have any idea what I need to do to keep GugaTagFixer's edits from flooding the Recent Changes list?  Apparently the "Hide bots" default isn't working, and I'm not sure why. --[[User:Short Circuit|Short Circuit]] 16:37, 3 February 2009 (UTC)
:I don't know why too, but you can delete some logs. --[[User:GugaTagFixer|GugaTagFixer]] 17:08, 3 February 2009 (UTC)
:: I don't care if it gets into the logs, I'm just annoyed that my primary means of following activity, the RSS feed, mostly consists of bot edit entries right now.  That can't be retconned.  Ah well, I'll figure it out later. --[[User:Short Circuit|Short Circuit]] 17:18, 3 February 2009 (UTC)
:::That last edit (posting here) by the bot didn't show up in the recent changes. --[[User:Mwn3d|Mwn3d]] 17:53, 3 February 2009 (UTC)
::::Yes, i fixed Case-Sensitive, Bot Flag, and Redirects (G++, GTK+, Visual C++, RCBF/C++). --[[User:GugaTagFixer|GugaTagFixer]] 17:55, 3 February 2009 (UTC)
:::Heh.  Indeed, it looks like it was fixed before I even mentioned it. :-)
