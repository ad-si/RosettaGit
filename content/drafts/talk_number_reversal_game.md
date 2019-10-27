+++
title = "Talk:Number reversal game"
description = ""
date = 2010-08-09T06:30:23Z
aliases = []
[extra]
id = 6746
[taxonomies]
categories = []
tags = []
+++

This seems to be a variation of what WP calls [[wp:Pancake sorting|Pancake sorting]]. Should the task perhaps be revised somehow? Or perhaps a new "pancake sorting" task be added, and then note this as being ''applied'' pancake sorting? -- [[User:Eriksiers|Eriksiers]] 20:02, 4 April 2010 (UTC)

: Hi Eriksiers, the only restriction on the method of sorting is that only reversals are allowed. It could be that pancake sorting might be a way to get to the desired result, but that algorithm is not mandated. The program does not ''do'' a sort. --[[User:Paddy3118|Paddy3118]] 20:47, 4 April 2010 (UTC)
:: What I meant is that I think that it makes the ''player'' to perform a pancake sort, kinda... Shrug. It was just a thought. -- [[User:Eriksiers|Eriksiers]] 01:38, 5 April 2010 (UTC)
::: Whoa, I see what you mean though. Maybe a C.f. ? --[[User:Paddy3118|Paddy3118]] 05:35, 5 April 2010 (UTC)
:::: What you did looks good to me. I'm too tired right now to give it any more thought tonight. -- [[User:Eriksiers|Eriksiers]] 06:37, 5 April 2010 (UTC)

==What Happens if the initial shuffle returns the list in sorted order?==
The Python entry is set up to handle this (rare, but still possible), case. Will other entries silently assume the player has won without doing anything? --[[User:Paddy3118|Paddy3118]] 17:21, 17 April 2010 (UTC)
: I don't know about the others, but the BASIC version doesn't check the order until after user input. All the user would have to do is press enter and they'd win with 0 moves, yes. The description and the implementations should be updated IMO; I'll take care of BASIC. -- [[User:Eriksiers|Erik Siers]] 18:56, 17 April 2010 (UTC)

:: Hi, I just saw yet another example added with the same flaw. I could add a note to the task description, but the task description seems to be OK. The problem is a subtle error in a lot of the implementations. I'll mark a few more as incorrect.

::: I think it's misleading to say "Example is likely to fail if numbers are initially randomly shuffled to the ordered state." The example simply doesn't implement the stipulated condition; if that is a failure, then the example certainly fails. Flag should say something like "Example should disallow the (unlikely) case that the list presented to the player is already ordered."
:::: To use your phrase: ''"The example simply doesn't implement the stipulated condition;''. The idea is to play the game. Whilst one could argue about the meaning of 'playing' or some-such, wouldn't it be better if the check were in? I don't agree that the word unlikely helps all that much. If the example isn't fixed and you think things are working then have to show it to people in authority, then it could prove embarrassing if they pick-up that it could randomly fail and that either
::::# You don't know why, or
::::# You do know why but then have to defend leaving in the bug.
::::(P.S. Please sign your talk page entries, thanks) --[[User:Paddy3118|Paddy3118]] 02:51, 2 August 2010 (UTC)
::::: I am not here objecting to the problem stipulation: I'm saying the text of the messages flagging versions as incorrect is unclear and misleading. I offered a suggestion for changing that text. If the problem is that an implementation is allowing an already sorted shuffle, then the message should clearly say so. -- Randy Hudson 2010-08-02
:::::: Oh. I'll take more care if I have to do the same again. Thanks. --[[User:Paddy3118|Paddy3118]] 18:52, 2 August 2010 (UTC)
So the issue is that problem statement calls for presenting an initial list that is definitely not in sorted order, and implementations should check for that. (I'm spelling it out because the above discussion doesn't, quite.)

: Hi 24.41.5.170, Yes. Without the check for an initial scramble to the sorted state you would get a program that worked most of the time, but every once in a while, you would start it and it would fail to allow you the pleasure of playing the game! I have started to think of this as my own design pattern to use when working with shuffled lists. (P.S. Please sign your talk page entries, thanks) --[[User:Paddy3118|Paddy3118]] 02:51, 2 August 2010 (UTC)

P.S. Sorry for the incorrect tagging of Factor. --[[User:Paddy3118|Paddy3118]] 06:30, 9 August 2010 (UTC)
