+++
title = "Talk:Play recorded sounds"
description = ""
date = 2010-11-13T08:04:41Z
aliases = []
[extra]
id = 4369
[taxonomies]
categories = []
tags = []
+++

==Task focus==
I think this task is massively overcomplex at the moment. Too many features mixed into one. This task should instead be just doing simple playback (and possibly mixing of multiple sounds) and the other bits moved to additional tasks. This will make it both easier to implement and easier to read for specific features. —[[User:Dkf|Donal Fellows]] 08:44, 11 June 2009 (UTC)
: Agree. Moreover, I find not so clear the required/suggested cathegorization: should we use as == header the "lib/system" used and as === the language? --[[User:ShinTakezou|ShinTakezou]] 15:28, 11 June 2009 (UTC)
:: I'd be tempted to say keep with the language focus used elsewhere. Overall site consistency is a good goal. :-) —[[User:Dkf|Donal Fellows]] 15:33, 11 June 2009 (UTC)

General reply: On reflection, it would probably be better to split up the task so the examples of any one are more all of the same thing. However, I don't think categorizing by language rather than audio facility is appropriate because audio is not ''usually'' a language feature and the details depend far more on the audio facility than what language you talk to it in. Also, [[User:Short Circuit]] has expressed interest (on IRC yesterday) in expanding Rosetta Code to other types of comparison: libraries, algorithms, paradigms. (And a clarification by example: The two examples currently on the page, categorized under C# and Tcl headings, are not contrary to my suggestion because they are using (I assume) language-provided audio libraries.) --[[User:Kevin Reid|Kevin Reid]] 19:26, 11 June 2009 (UTC)
: I think a split is warranted; The task is rather broad, as it currently stands.  I'm ambivalent about organizing by language or library for this particular task, because of (I hope) possible future changes to how displayed task example code is organized.  And, yes, I'm still very much in favor of additional forms of chrestomathy; The more effective means with which code on Rosetta Code can be organized, cross-referenced and compared, the better it serves its goals of comparing programming concepts, enabling developers and designers and educating newcomers. --[[User:Short Circuit|Short Circuit]] 19:49, 11 June 2009 (UTC)
:: Who wants to take on splitting this task into core components and related concepts? I'd like to see it deprecated and (eventually) removed.
:: Suggested replacements:
::* [[Audio notification]] -- generate some audible sound, nonspecific. Usage of, e.g. Win32 Beep() or tty beep code is perfectly acceptable here.
::* [[Audio playback]] -- play back some sound file. (I could provide the relevant clip, if needed.)
::** Optional stereographic and spatial perceptual placement. (Often applied in audio mastering and in games)
::* [[Audio recording]] -- Record a sound file.
:: The parts involving simultanous and glitch-free playback can come later, if needed. --[[User:Short Circuit|Michael Mol]] 13:03, 25 August 2010 (UTC)

==Playing on Linux==
Here's some discussion of playing recorded sounds in C on Linux using various libraries: http://blog.ometer.com/2010/11/11/playing-a-sound-file/  Perhaps some of the information there could be added to this task. —[[User:Kevin Reid|Kevin Reid]] 03:27, 13 November 2010 (UTC)
