+++
title = "Rosetta Code:Village Pump/Lang-tag bot"
description = ""
date = 2010-11-28T17:39:24Z
aliases = []
[extra]
id = 4976
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Lang-tag bot
|summary=Program to add lang tags and correct them to use the right identifier
}}
==Announcement==

I've just perfected [[Rosetta Code:Village Pump/Lang-tag bot/Source|UnderBot]], a program to add lang tags where they're missing and fix them where they're present. By fixing lang tags, I mean changing them to use the right identifier (e.g., "cfm" rather than "coldfusion") and getting rid of excessive whitespace. UnderBot isn't perfect, since it has to guess a great deal, but I've polished it to the point that I think unleashing it upon Rosetta Code would fix a lot more than it would break. Here are UnderBot's last six edits:

* [http://rosettacode.org/mw/index.php?title=User_Output_-_text&curid=1514&diff=67713&oldid=65938 User Output - text]
* [http://rosettacode.org/mw/index.php?title=True/False_Values&curid=4490&diff=67714&oldid=66923 True/False Values]
* [http://rosettacode.org/mw/index.php?title=Collections&curid=1635&diff=67716&oldid=67530 Collections]
* [http://rosettacode.org/mw/index.php?title=24_game_Player&curid=4940&diff=67723&oldid=67707 24 game Player]
* [http://rosettacode.org/mw/index.php?title=Abstract_type&curid=3131&diff=67724&oldid=67282 Abstract type]
* [http://rosettacode.org/mw/index.php?title=Ackermann_Function&curid=3050&diff=67725&oldid=66691 Ackermann Function]

(Full disclosure: I did manually change a few characters in one of these edits. But it was one of those special cases that's so rare it's not worth implementing.)

So, I just want to ask permission of Rosetta Code in general, and Mike Mol in particular, to let this program edit [[Rosetta Code:Village Pump/Lang-tag bot/Task list|330 pages]] without supervision. I also want to ask what's a good number of seconds (or minutes, or hours) to pause between edits so as to avoid excessive strain on the server. Oh, and I request that dummy language files for ALGOL 68, APL, and TI-89 BASIC be installed into Rosetta Code's GeSHi (just make copies of <code>text.php</code> and name them <code>algol68.php</code>, <code>apl.php</code>, and <code>ti89b.php</code>), to guard against [[Rosetta Code:Village Pump/Syntax Highlighting#Non-ASCII characters and unrecognized languages|this bug]].

Lastly, everyone: even if you don't know Perl or don't want to check the bulk of my code, glance at the <code>%langtags</code> hash in [[Rosetta Code:Village Pump/Lang-tag bot/Source|the source]] and see if you agree with the lang-tag identifiers I've chosen for the languages you know. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 20:03, 14 November 2009 (UTC)

:Can you put a kill switch in place?  Such as if a string appears on a particular page?  Then give it an edit rate of one every ten minutes or so. If someone sees it misbehaving, they can trigger the kill behavior. --[[User:Short Circuit|Michael Mol]] 23:50, 14 November 2009 (UTC)

::Okay. Now, if you insert the string "stopediting" [[User talk:UnderBot|here]], UnderBot will stop editing. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 16:02, 15 November 2009 (UTC)

::: IIRC (really not sure) it's standard on Wikipedia that ''any'' edit to the talk page stops the bot. --[[User:Kevin Reid|Kevin Reid]] 18:20, 15 November 2009 (UTC)
::: Do you have a user for the bot?  I'll give it bot privs, which lets it use the API. --[[User:Short Circuit|Michael Mol]] 21:11, 15 November 2009 (UTC)
:::: nm.  I'm some kind of idiot, I guess.  It's got privs now. --[[User:Short Circuit|Michael Mol]] 21:12, 15 November 2009 (UTC)
: As of this morning, [[Special:Contributions/UnderBot|UnderBot is active]]. I turn off my computer at night, so it'll only be editing during the day. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:45, 19 November 2009 (UTC)
:: Okay, it's done. If in the future anybody wants me to run UnderBot (or something like it) on a particular page or set of pages, just ask. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 21:22, 22 November 2009 (UTC)

==Delay after null edits==

When this bot finds a page that it doesn't need to edit, does it immediately seek out another page or does it wait for the next interval? If it waits, maybe you could shorten the wait when it doesn't edit to something like 30 seconds instead of ten minutes. --[[User:Mwn3d|Mwn3d]] 20:14, 20 November 2009 (UTC)
:It waits the full ten minutes, just because I didn't bother to make it smart enough to do otherwise. I figure there's no hurry. Pages that don't need any changes are relatively rare (out of nearly fifty pages UnderBot has looked at so far today, it left only one unchanged), and the whole job will be done before (the American) Thanksgiving in any case. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 21:14, 20 November 2009 (UTC)

== Change to correct case? ==
My impression is that the lang tags are case-sensitive? The bot doesn't seem to fix lang tags that have incorrect case -for example [[File Size#J]]. Are adjustments required?--[[User:Tikkanz|Tikkanz]] 21:49, 20 November 2009 (UTC)
:i think lang tags are not case-sensitive, but it's nice to have them standardized. --[[User:Mwn3d|Mwn3d]] 21:54, 20 November 2009 (UTC)
:Lang tags are case-insensitive, as Mwn3d pointed out to me earlier. The bot preserves the capitalization of existing tags, so long as they use the right language name (for instance, "lisp" rather than "clojure"), to keep from producing a hundred useless difference regions converting "AutoHotkey" to "autohotkey". When it adds new tags, it arbitrarily uses all lowercase. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:49, 20 November 2009 (UTC)

== Handle nowiki tags ==
A bit late now that Underbot's finished, but a potential future enhancement might be to remove <nowiki><nowiki></nowiki> tags from inside blocks of code when Underbot adds missing <nowiki><lang></nowiki> tags.--[[User:Tikkanz|Tikkanz]] 21:35, 22 November 2009 (UTC)
:It does remove &lt;nowiki&gt;s when they're surrounding an entire block of code (see line 287 of the source), but not in any other case. I'd (wrongly) supposed nobody would enclose only part of their code in &lt;nowiki&gt;s, since it would be easier to just &lt;nowiki&gt; the whole thing. Oh, well. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:31, 22 November 2009 (UTC)

==TI-89 BASIC==
Until such time as GeSHI integration on RC doesn't mangle all non-ASCII characters, it must not be used for TI-89 BASIC (or APL etc.). Please change UnderBot to, in the particular case of TI-89 BASIC, use instead the tags <code><nowiki><pre style="font-family:'TI Uni'">...
```
</nowiki></code>, and rerun it, as it has broken most of the TI-89 examples. Besides ensuring that the TI proprietary characters show up properly given the font, the styled pre is also a recognizable sequence which we can change back to a specific lang tag once GeSHI isn't broken. 

Also, I think it's a bad idea in general for the bot to strip out tags with attributes it doesn't expect (pre style= here), since they indicate some special situation. —[[User:Kevin Reid|Kevin Reid]] 17:22, 1 December 2009 (UTC)
: I think it makes more sense for Short Circuit to either
:* install a stub language definition for TI-89 BASIC, as I explained above (this would be extremely easy! I'd do it myself, but the permissions on the relevant directory won't let me), or
:* fix [[Rosetta Code:Village Pump/Syntax Highlighting#Non-ASCII characters and unrecognized languages|this bug]].
:Changing all that markup just to install a bug workaround which would then have to be reversed when the bug is fixed would be, in my humble opinion, completely boneheaded. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 21:00, 1 December 2009 (UTC)
::Ah, I missed that GeSHI is itself Unicode-clean and it is the no-highlighting path at fault. Fixing the bug is of course preferable to preserving or generating nonstandard markup; I only claim that ''the bug should have been fixed first'', and we should now either fix the bug or have the bot change the markup, whichever will happen sooner, since the affected examples are currently unreadable.
::However, I do feel that your bot did two, ah, boneheaded things:
::* The Unicode problem is known. The bot's table of lang codes should not include languages which use non-ASCII characters and do not yet have definitions, until the bug is fixed.
::* The bot should not discard markup (style=) it does not understand; it should assume that there is some special situation and not make any changes to that code block.
::(Note that even if GeSHI gets TI-89 support there would still need to be a special case (whether in RC/GeSHI or in the markup) to specify the font.) --[[User:Kevin Reid|Kevin Reid]] 21:59, 1 December 2009 (UTC)
:::Short Circuit fixed the bug. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 01:24, 2 December 2009 (UTC)
