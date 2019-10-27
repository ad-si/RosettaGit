+++
title = "Rosetta Code:Village Pump/FBSL Categorization Question"
description = ""
date = 2013-11-13T13:57:32Z
aliases = []
[extra]
id = 16677
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=FBSL Categorization Question
|summary=Why not let FBSL stay alone?
}}

Hello community,

I've just noticed some activity going on to group various BASIC dialects into a common BASIC branch in the language tree of certain tasks. It being generally a positive idea which obviously targets better readability and ease of language tree navigation, I wouldn't however consider it reasonable to place such true multi-syntax languages as [[FBSL]], or interoperative languages like [http://terralang.org/ Terra], [http://luajit.org/dynasm.html LuaJIT/DynASM] and a few others, into any particular side branch of the tree. They are too, well, extraordinary for that, and an unbecoming neighborhood may do such languages a disservice of misleading their potential users. Yup, it '''is''' difficult to be different, as [[User:Paddy3118|Paddy3118]] once said. ;)

[[FBSL]] is exactly as much "BASIC" as it is "assembly" or "C". Why aren't assemblers grouped in a branch? Why aren't C clones (C, C++, C#) grouped either? Markup languages? Esoteric languages? Then why not let FBSL stay alone in the main trunk? At least for fear lest someone rewrite the FBSL solutions in FBSL's thoroughbred Dynamic C and claim C kindred. :)

Kind regards, --TheWatcher 03:58, 11 November 2013 (UTC)

: ... not to mention the various   LISP   flavors. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:29, 11 November 2013 (UTC)

:: I don't think there's any problem at all in having FBSL entries in their own right, provided the idiomatic way of achieving the task at question is significantly different in FBSL than other BASIC variants. (Goodness knows, they vary a ''lot''.) The difference could even be down to different typical availability of libraries (though if that's the case, it should be documented with {{tmpl|libheader}} of course).
:: All I'd like to avoid is having the same thing written out multiple times, or entries that just say “see this other entry”. The former is wasteful, and the latter sucks as the users of the ''other'' language won't easily see that there's strong syntactic similarities; we can use multiple {{tmpl|header}} macros in one heading if necessary, and that creates better usability and discoverability. (This is what I was encouraging the Icon/Unicon entries to do; AIUI, they're often the same, but not always.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:27, 12 November 2013 (UTC)

::: @Donal Fellows
::: If I get you correctly, an FBSL entry may only stay alone if it shows anything that other BASIC's can't do? I assume you've already had a [[Anagrams#FBSL|quick look here]] or [[Ackermann_function#FBSL|here]]. Let me assure you each and every entry may be (re)written in exactly that style ''without any stand-alone libraries'' or include files with extra code. ''Each and every portion of the script'' down to a separate statement may be written in ''one of the three constituent languages'' - BASIC script (extended BASIC), Intel assembly, or ANSI C. Everything is there in ''one and the same 600KB Fbsl.exe binary''. The only general-purpose include file we've found helpful so far is <Windows.inc> which hosts about 7,000 standard Windows #Define macros. This is because around ''2,300 Windows API's'' from system-wide kernel32.dll, user32.dll and gdi32.dll are ''always available'' to the FBSL users transparently as if they were just part of the native vocabulary.
::: If it boils down to being different in each and every entry just by showing off that unique feature again and again instead of the user or criticist looking it up just once in the Category:FBSL entry, I ''will'' re-write all my submissions in that style for FBSL to remain in the language tree trunk once and for all. Regards, --TheWatcher 23:57, 12 November 2013 (UTC)
-----

In the same vein, one of those updates/consolidations managed to delete two program (solution) entries, namely a   '''Batch File'''   and an   '''XPL0'''   entry. 

I made a mention of this in the talk section of the Rosetta Code task (in the same day or next morning that it happened), but so far, those two language entries haven't been repaired or re-instated.   To make it worse, I have at this point forgotten which task it was, but it was one of the first attempts at the consolidation/grouping of the   BASIC   languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:29, 11 November 2013 (UTC)

-----
I've been consolidating BASICs as I've been making contributions.  I will stop doing this because it seems too error prone, and it sounds like some BASICs really have very little relation to BASIC at all.  This relates to old discussions about BASIC:  http://rosettacode.org/wiki/Category_talk:BASIC

It would be good to consolidate the very similar solutions but I imagine that would be error prone too.  I repaired one of my entries where XPL0 moved, I since moved it to where it belongs alphabetically.  http://rosettacode.org/wiki/Hailstone_sequence

-- [[User:mmphosis|mmphosis]] ([[User talk:mmphosis|talk]]) 4:51, 13 November, 2013 (UTC)

: Hehe Mmphosis,
: So it '''was''' you who started that conspiracy! :) I won't reinstate my original message to you as you can still read it on your talk history page, and it was too hot-spirited anyway. My bad. :) But I do appreciate your assuming the responsibility for the mess.
: As for the BASIC's namespace, the idea seems reasonable though one should realise there are BASIC's and BASIC's. For instance, FBSL doesn't position itself as a BASIC but rather as a scripting language as is clearly stated by its name. FBSL can have a subtree of its own with separate branches for BASIC-style, assembly-style, C-style, and mixed solutions. Personally, I prefer to keep things simple and I wouldn't bloat my code with C or assembly just to spawn a window or recode a BMP into a PPM. Nor would I use OOP if I can avoid it. As long as FBSL supports it, it will always be my own prerogative which particular methods I choose. But some other user coming up here with their own solution should have a chance to upload their assembly code version too, and it only seems reasonable that they shouldn't do it in the BASIC branch of the tree.
: This was the main motive for my message that I wanted to get through.
: Regards, --TheWatcher 10:53, 13 November 2013 (UTC)
