+++
title = "Category talk:GML"
description = ""
date = 2011-01-28T19:33:46Z
aliases = []
[extra]
id = 9189
[taxonomies]
categories = []
tags = []
+++

=HOPL=

The HOPL link is for a completely different language. Any way to turn it off for just this page? -- [[User:Eriksiers|Erik Siers]] 23:26, 27 January 2011 (UTC)

: Part of the issue is that the HOPL doesn't cover this variation on “GML”. The right thing would be to get the HOPL extended, surely? –[[User:Dkf|Donal Fellows]] 09:44, 28 January 2011 (UTC)
:: Poke [[User:Axtens|Axtens]]; he's part of the HOPL core folk, I believe. Also poke [[User:Mwn3d|Mwn3d]], and see if {{tmpl|language}} can be tweaked to customize the HOPL link. --[[User:Short Circuit|Michael Mol]] 14:35, 28 January 2011 (UTC)
:::I got rid of the link but I need to do some more crazy stuff to get rid of "See also:". I'll work on it more later. --[[User:Mwn3d|Mwn3d]] 15:28, 28 January 2011 (UTC)
::::How about moving "See also:" to the [[Template:HOPL|HOPL template]]? I '''think''' that would take care of it, if I understand mediawiki markup rightly. -- [[User:Eriksiers|Erik Siers]] 16:37, 28 January 2011 (UTC)
:::::Well yeah, but there are other links that show up in the "See also" section. There are links to [[Language Comparison Table|the LCT]] and links to a BNF for the language that can be there. I want the "See also:" to show up if at least one of those is present. That'll take some thinking.--[[User:Mwn3d|Mwn3d]] 17:07, 28 January 2011 (UTC)
::::::Ah, I see.
::::::Does mediawiki support anything like '''<code>if x or y ...</code>''' or '''<code>if x ... elseif y ...</code>'''? (The mw docs aren't helping me much.) I'm thinking that something simple like '''<code>if HOPL or LCT or BNF then "See also:"</code>''' or '''<code>if HOPL then "See also:" elseif LCT then "See also:" elseif BNF then "See also:"</code>''' might work (in appropriate mw markup, of course).
::::::If neither construct is available, it might be worth it to extend the mw source. (I'm not offering to do it myself, though; PHP is pretty low on my list.) -- [[User:Eriksiers|Erik Siers]] 18:32, 28 January 2011 (UTC)
:::::::Yeah I have something like that in there right now, but it needs tweaking. It has to be "if hopl=yes or LCT or BNF". Basically I didn't feel like messing with nesting right now. I've been looking [http://www.mediawiki.org/wiki/Help:Extension:ParserFunctions here] for help. If you want to give it a try you can. Try things out in {{tmpl|Language beta}} and see what happens. Actually we should look at bringing in some of the sweet-looking table styling from that template... --[[User:Mwn3d|Mwn3d]] 19:18, 28 January 2011 (UTC)
::::::::That help page looks like a nightmare. :-) -- [[User:Eriksiers|Erik Siers]] 19:33, 28 January 2011 (UTC)
:::::::I think I just got it. It looks right here and I checked [[Java]] to see if it broke other things and it looks OK. Let me know if you see problems. --[[User:Mwn3d|Mwn3d]] 19:32, 28 January 2011 (UTC)
::::::::Looks good to me. -- [[User:Eriksiers|Erik Siers]] 19:33, 28 January 2011 (UTC)
