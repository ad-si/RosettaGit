+++
title = "Talk:Discordian date"
description = ""
date = 2015-10-13T02:20:19Z
aliases = []
[extra]
id = 7785
[taxonomies]
categories = []
tags = []
+++

According to the wiki page, the Disscordian calendar corresponds exactly to the Gregorian calendar (with some contrarian disputes about corresponding to the Julian calendar).  The significant difference is in naming.

It would be good for the task itself to specify the naming required, rather than requiring us to extract them from that wiki page.

But, also, if dates are represented numerically, the identity function is sufficient to convert a gregorian date to a discordian date, except that the year needs to be adjusted.

Thus, at present, this task is underspecified.

:Actually it's a bit different in that it lacks months and instead splits the year into five seasons of 73 days each, St. Tib's intentionally being the odd one out and complicating a numerical sequential representation. It's linked to the Gregorian date apart from that, though (i.e. New Year's Day is identical, the number of days in a year is identical and the year has a static offset).
:The naming isn't particularly important, though there's a convention to use them instead of representing the seasons by their ordinals.
:I agree that the task description is incomplete, though. It'd probably be a good idea to at least specify the expected format ("$season $d, YOLD $yyyy"). --- [[Special:Contributions/78.35.107.83|78.35.107.83]] 16:40, 27 July 2010 (UTC)
::For now, I have updated the J implementation to use season ordinals, and to insert an imaginary day after the 59th of the first season, on leap years. --[[User:Rdm|Rdm]] 17:01, 16 August 2010 (UTC)

--[[User:Rdm|Rdm]] 14:52, 21 July 2010 (UTC)
: I agree that simply linking over to a WP for even basic details makes for a bad description. Would someone create a template for marking such tasks, and then find and tag the tasks that do that? If the tagging is done first, more people can concurrently assist with actual cleanup. --[[User:Short Circuit|Michael Mol]] 17:17, 22 July 2010 (UTC)

== Leap Years ==

According to wikipedia page, every fourth year is a leap year. This diverges from the Gregorian, which has special rules for years divisible by 100. Should the code respect this or just ignore it? --[[User:Oenone|Oenone]] 08:27, 21 January 2011 (UTC)

: Match the Gregorian year definition, as trying to deal with other kinds of calendar turns the task into a ridiculously awkward one. –[[User:Dkf|Donal Fellows]] 09:30, 21 January 2011 (UTC)

:Normally the calendar matches the Gregorian. However, there is a variant (less common than the normal one) which does diverge from the Gregorian as mentioned above (due to a slight ambiguity in the Principia Discordia). The task should probably just use the Gregorian-based version, although it should be allowed for a code to support both versions if someone want to write such a code, too. --[[User:Zzo38|Zzo38]] ([[User talk:Zzo38|talk]]) 02:20, 13 October 2015 (UTC)

== D Entry ==

Some rationale for the changes in the D entry:

- For browser screens with a not high resolution it's better to keep lines of code short. For Rosettacode code I use about 72-73 chars max for line.

- To keep code readable it's usually better to keep the then/else clauses on their own lines.

- Variables should be immutable (or const, where immutable is not possible) unless they have to mutate or unless some compiler bug (or the design of ranges) makes immutable/const unusable.

- The scope of variables should be minimized. So arrays like 'seasons' can go in the function scope and avoid polluting the module namespace.
: Since these constants are fundamental to Discordian dates, they are more useful in the module namespace: other modules could query them and new functions added to the module would be able to see them immediately. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 15:15, 25 July 2013 (UTC)

- Some variable names like dYear now have an upper case in the middle to underline their inner words. It's normal D style.

- Currently the format() function is not pure nor nothrow, but maybe in future it will become pure. So I keep some commented out code like /*pure*/ that are meant to be replaced by code in future if and when Phobos improves (I replace such comments all the time in Rosettacode entries because Phobos keeps improving).

== Broken examples ==

It seems this task has been rather poorly implemented, possibly because one of the original examples was incorrect, and others have just translated that directly. I've marked about a dozen as incorrect so far (some of which have already been fixed), but I'm fairly sure there are more that are wrong.

The easiest mistake to spot is anything that calculates the day number with a simple modulo operation since that is bound to give you a zero day at some point, which would never be correct. Examples that appear to have this mistake that I haven't flagged yet include [[Discordian_date#Euphoria|Euphoria]], [[Discordian_date#Mathematica|Mathematica]], [[Discordian_date#PureBasic|PureBasic]] and [[Discordian_date#Racket|Racket]]. I don't want to mark anything incorrect that I haven't been able to verify directly, though, so I'm hoping others can confirm whether those are right or wrong.

The handling of leap years seem to be another source of confusion, so that's also something worth testing thoroughly if you want to be sure that your example is truly correct.

--[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 12:38, 7 October 2015 (UTC)
