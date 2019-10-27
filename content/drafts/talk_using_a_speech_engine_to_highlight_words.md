+++
title = "Talk:Using a Speech engine to highlight words"
description = ""
date = 2018-12-26T01:53:11Z
aliases = []
[extra]
id = 9416
[taxonomies]
categories = []
tags = []
+++

==Problems==
I think their may be problems with the task as their will be too few languages with in-built speech-recognition features. I suggest we delete the task and the original task creator add the suggestion to [[Rosetta Code:Village Pump/Suggest a programming task]] maybe?

Unless two or more languages have the ability, the task will devolve into ways to call an external library - for which we already have tasks, and it will hardly help comparing ''languages''.

In summary, I think the task may break the [[Rosetta_Code:Add_a_Task#Things_to_avoid|guidelines]] section "Don't require exceedingly rare features.". --[[User:Paddy3118|Paddy3118]] 12:25, 4 April 2011 (UTC)
: I don't have a problem with tasks which wind up primarily calling out to libraries. Seeing how a language interacts with a particular library isn't necessarily useful in comparing a language to another language, but it is still useful for users of that language and users of that library, and may be useful in comparing libraries. Utility to users of a particular language is a significant language-community motivator for adding code to the wiki, and I've seen several languages whose communities get together to add to RC for that purpose. As for comparisons between libraries...that's something I've wanted RC to be able to cover for years. Like languages, libraries are another tool where active competition and code and idea development occur.
:
: The bit about "don't require exceedingly rare features" was not intended to excluding calling outside the language's native semantic space. It was more intended to deal with tasks like [[Proof]] which have a very narrow definition and are written to be intolerant of non-native features. --[[User:Short Circuit|Michael Mol]] 12:59, 4 April 2011 (UTC)

:: Thanks for the clarification Michael. --[[User:Paddy3118|Paddy3118]] 14:01, 4 April 2011 (UTC)

Yeah, sorry the article is about computerized speech, not speech recognition. The task was badly worked, because I rushed this article through on a dinner time, whilst demostrating Rosetta Code to a colleague at work. I have fixed this now.

I am not entirely happy with the title. Maybe this should read "Highlight each word as is it spoken by the speech engine".

[[User:Markhobley|Markhobley]] 16:47, 4 April 2011 (UTC)

I suspect that we ought to do a more basic speech-related task first, such as saying a constant string like “Hello, world!” Without being able to do that, there's no way to take on a much more challenging task like this one. –[[User:Dkf|Donal Fellows]] 17:21, 24 April 2011 (UTC)

: I've reworded the task description to try to match better what I believe you intended by it (and to make it clearer to everyone what was actually meant so we can judge whether solutions make the grade). Please feel free to change it if you think I've not captured your intent. –[[User:Dkf|Donal Fellows]] 11:08, 11 September 2011 (UTC)

:: I agree about renaming it.   As it stands now, it appears that the speech engine is supposed to do the highlighting   (rather than the computer program that is being used).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:50, 26 December 2018 (UTC)

:: Also, I suppose not many (at best, just a few) computer programming languages would have (native) speech synthesis.   I took the task's requirement such that the computer program could call another speech engine to do the heavy lifting.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:50, 26 December 2018 (UTC)

==Remove and add to suggested tasks list?==
It has been several weeks without the first language implementation. Without one, what is the difference between this task and one on the long list of requests for tasks?

Maybe it should be retired there? --[[User:Paddy3118|Paddy3118]] 18:17, 24 April 2011 (UTC)

:It is easy enough to write an implementation. Apparently VB6 has this facility built into the speech engine, enabling it to be achieved by a programmer with an beginner/intermediate level of skill. (I am not a developer for that platform though, so I haven't got any sample code for this at this time.)

:For other engines, we can emulate this with a parse, highlight and speak loop. As long as we can control the cursor, this should be easy. I'll knock some code together for other languages as time permits. [[User:Markhobley|Markhobley]] 18:48, 24 April 2011 (UTC)

Hi Mark, It is not whether or not the task is easy or not to write that bothers me, it is that no one thinks it's easy enough to have gone ahead and done it after several weeks. Normally the first implementation follows within a day of the task description; and it's usually done by the person who writes the task description so they can get a good task description and first implementation out for others to follow. I think this task is only at the suggestion stage. Thats why I think it isn't yet ready for even a draft task. Maybe others would like to chip in with their thoughts? --[[User:Paddy3118|Paddy3118]] 21:24, 24 April 2011 (UTC)
:I'm not sure having the task there will get it implemented either. I agree that 3 weeks is a pretty long time for a task to be sitting around not gaining anything. I don't even think having sample implementations will do much good unless there is a cross-language library for it. It seems destined to be a slow-moving task. I'm not sure what that all means for whether it should be here, on the request list, or even on the site. Take from it what you will. --[[User:Mwn3d|Mwn3d]] 17:01, 25 April 2011 (UTC)

: I'd rather have everything on the "suggested tasks" page set up as "alpha"-grade tasks, have the current "draft" tasks sit somewhere in the range of alpha and beta, and have full tasks as confident beta and beyond. The "suggested tasks" page is where a lot of things simply go to die. I don't think anyone looks at that list when looking for inspiration for something to implement. I'd rather not have it at all. There's a great likelihood of duplicates in a "straight-to-alpha" task model, but I think the subsequent discussion around comparing and contrasting related tasks will ultimately result in a better task selection. --[[User:Short Circuit|Michael Mol]] 17:23, 25 April 2011 (UTC)

== Implemented in AutoHotkey ==

I have implemented this task in AutoHotkey. It is possible that I can translate that into VBScript and possibly js because of the COM similarities, however I'd still need a way to "highlight" the text. AutoIt should be fairly simple to forge out of this (if only I knew it)
I find this task very interesting and would like to see it salvaged. I'm watching this page so please provide feedback/ideas on my implementation/other possible implementations :) --[[User:Crazyfirex|Crazyfirex]] 02:45, 10 September 2011 (UTC)

:I am going to be developing some specifications for behaviour highlight follower type applications. It is just a task on my enormous todo list at the moment though, so I haven't got anything to contribute at this time. Basically the specification will cover positioning of the highlighter bar as text is scrolled up or down, etc. I used to write such applications many many years ago. Unfortunately all of my applications were written in assembly language over MSDOS, and I used my own windowing engine. so I am having to reengineer everything from ground up to make anything usable today. [[User:Markhobley|Markhobley]] 09:14, 10 September 2011 (UTC)

== Link to Speech Synthesis? ==

I think it would be helpful to link the description to the Speech Synthesis task, and possibly others such as Gui creation. By the way, in languages like VBScript which cannot create Gui's, is it acceptable to print each word to stdout?

: I've added the link, and also vice-versa.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:52, 26 December 2018 (UTC)
