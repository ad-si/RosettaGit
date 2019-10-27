+++
title = "Rosetta Code talk:My Scripts Menu"
description = ""
date = 2010-07-19T19:14:30Z
aliases = []
[extra]
id = 7779
[taxonomies]
categories = []
tags = []
+++

This doesn't seem to save my script preferences between pages. I might have weird cookie settings though. I use Spybot S&D and it configures some cookie settings. In any case, the scripts work right after I check the box so it's not much of a problem. --[[User:Mwn3d|Mwn3d]] 00:16, 19 July 2010 (UTC)

: So far I've only tested it in Firefox, so it's entirely possible it's a bug.  What browser and operating system are you using?  --[[User:Tyrok1|Tyrok1]] 00:53, 19 July 2010 (UTC)
::FF 3.6.6, Windows 7 (32 bit). --[[User:Mwn3d|Mwn3d]] 00:56, 19 July 2010 (UTC)
::Not that I expected it to, but it doesn't seem to save on Dolphin Browser HD 2.1 on Android 2.1. --[[User:Mwn3d|Mwn3d]] 01:10, 19 July 2010 (UTC)
::: So far I've tested it in Firefox, Chromium, Epiphany, Midori, all of which seem to work just fine for me.  Is it giving you any errors or anything that would indicate it's running abnormally?  After you check a checkbox, if you look at the cookie ([ see this link for a howto]), is there anything stored in it under rosettacode.org under the key "jsGadgets"?  --[[User:Tyrok1|Tyrok1]] 02:15, 19 July 2010 (UTC)
::::That website's directions didn't match what I saw in the menus. I used [ this] instead. I see a cookie named "jsGadgets" with the value "LanguageComparison" after I click the first box. The value changes to "LanguageComparison%2CUtilityButtonBar" after I click the second box. When I reload the page the cookie is still there, but the buttons aren't checked and the page looks like default. Same for when I click around to a different task. --[[User:Mwn3d|Mwn3d]] 04:08, 19 July 2010 (UTC)
::::: Ah, that makes some sense.  The code's missing an unescape() call.  Looks like I've got to shorten up the line lengths, too.  Should be a pretty easy fix.  --[[User:Tyrok1|Tyrok1]] 15:16, 19 July 2010 (UTC)
::::: There we go - that should do it.  Make sure you clear your cache so you get the new version, and if you still have trouble with it, please let me know.  --[[User:Tyrok1|Tyrok1]] 19:09, 19 July 2010 (UTC)

Looks good now. I'll be on the lookout for more bugs. Thanks for fixing it. --[[User:Mwn3d|Mwn3d]] 19:14, 19 July 2010 (UTC)
