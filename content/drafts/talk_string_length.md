+++
title = "Talk:String length"
description = ""
date = 2014-08-30T08:03:58Z
aliases = []
[extra]
id = 2478
[taxonomies]
categories = []
tags = []
+++

==byte/character separation==
My first thought when I saw [[String Byte Length]] and [[String Character Length]] merged was, "Oh no, we've been through this."  But I see that each language is subsected for byte and character length, so this might actually be an improvement; It should be more obvious to passing contributors that there is a distinction. --[[User:Short Circuit|Short Circuit]] 16:48, 20 January 2008 (MST)

== Lua why not ==

... Again I hit my head against a copyright-left-up-down problem trying to fix the Lua code... I've found an interesting approach in a [http://lua-users.org/wiki/LuaUnicode Lua users wiki] (at least for UTF-8); the idea is basic and reproducible in other languages... but not for Lua now, since I've seen it on a wiki where the copy&larr;&rarr;&uarr;&darr; is not explicitly given... digging a while I've found a long thread ending more or less [http://lua-users.org/lists/lua-l/2006-10/msg00470.html here]. But after almost 3 year they still haven't an explicit copy* statement on the wiki... It seems the material can just be read, or I should try to dig the history of the page (not so easily accessible from the web, it seems) to see who wrote that snippet... (feeling defeated by strange laws and ...) --[[User:ShinTakezou|ShinTakezou]] 22:48, 16 April 2009 (UTC)

== And a way for ==

Examples need to use non ascii chars into code, but the lang tag does not permit it... so that møøse looks like mÃ¸Ã¸se ... I've fixed wørld in AWK just because it showed an example from command line, where pre tag is reasonable... but it is not so for J e.g., where back to pre from lang tag would just be... a step back as said... Shouldn't exist a strange way to fix it? (I believe it is enough to allow UTF-8 encoding in lang tag... keywords are mostly ASCII, just APL should be checked... in fact, how does APL encoding work?) --[[User:ShinTakezou|ShinTakezou]] 23:05, 16 April 2009 (UTC)
: Likely a GeSHi bug.  I'll drop them a line. --[[User:Short Circuit|Short Circuit]] 06:15, 17 April 2009 (UTC)

== Component Pascal ==

The example for character length does not deal with utf-8 and as much as I understand also fails with Non-BMP code points.

== Incorrect ==

The byte length calculations for unicode appear generally incorrect. They're only valid for codepoints which are in the Basic Multilingual Plane, but not for the Supplemental planes. I.e. 🀁 wouldn't fit within a single wide character; it would be represented in UTF-16 as 0xD38C and 0xDC01 (if I've done the math right). --[[User:Short Circuit|Michael Mol]] 18:24, 15 March 2012 (UTC)
: If you want to be completely general, there exist [https://en.wikipedia.org/wiki/Unicode_normalization other issues to consider.]  Note, in particular, that not all combining forms have codepoints.  --[[User:Rdm|Rdm]] 18:31, 15 March 2012 (UTC)
:: could we see a text file that contains the various BYTE strings and the expected length results? ..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:03, 30 August 2014 (UTC)

== PL/I error ==

the last line ( put skip list ('Byte length=', length(trim(SM)); )

is syntactically incorrect (a closing parenthesis is missing)

I tried to add it and get an error message:

IBM1569I S       9.0    SIZE argument must be a CONNECTED reference.
 
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:43, 22 October 2013 (UTC)
