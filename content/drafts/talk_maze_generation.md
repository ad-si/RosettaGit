+++
title = "Talk:Maze generation"
description = ""
date = 2017-12-08T10:13:21Z
aliases = []
[extra]
id = 8978
[taxonomies]
categories = []
tags = []
+++

==Refining the task==
Which algorithm? The linked wikipedia article has many. –[[User:Dkf|Donal Fellows]] 13:24, 14 December 2010 (UTC)
:I am quite happy about the freedom to use any algorithm. I am just worried about every example being accompanied with a large maze. --[[User:Paddy3118|Paddy3118]] 14:13, 14 December 2010 (UTC)
:Agreed. I also don't see the need to dictate a special algorithm. Its suitasbility might well depend on the language. --[[User:Abu|Abu]] 14:48, 14 December 2010 (UTC)
:You are right about the large examples. I reduced the size to just 6 lines. --[[User:Abu|Abu]] 14:58, 14 December 2010 (UTC)

:I recommend:
:* Specifying a maze dimension, depending on output format. If it's going to be ASCII art, make it, e.g. 40x30. If it's going to be a raster image that can be embedded, make it 640x480. (Actually, for the ASCII art, I'd suggest using whatever the terminal size was for old 40-column-wide terminals.)
:* Required that the algorithm used be identified, if possible. ("freestyle/homegrown" is an option, of course.)
:* As/when the page gets excessively, split into per-algorithm subtasks, so the particular algorithms can be compared.
:* As the subtasks again get large, split each examples' output into a separate page.
:* If example ''code'' is large, break the code out to its own subpage, as is done with tasks like [[RCBF]].
:I think that will allow the task to grow, and offers a reasonable balance of comparison, example freedom and page size along the way. --[[User:Short Circuit|Michael Mol]] 15:31, 14 December 2010 (UTC)
::40x30 is way too big. I've just changed the example output to 18x6. --[[User:Abu|Abu]] 15:37, 14 December 2010 (UTC)
::: Ah, I see that I'd assumed a 'filled/void' model. Too much Wolfenstein 3D map editing as a kid, I suppose. --[[User:Short Circuit|Michael Mol]] 15:54, 14 December 2010 (UTC)
::::On the other hand, 18x6 is too small, as the maze becomes almost linear. I'll change the initial example back to 16x12, sorry for the confusion. --[[User:Abu|Abu]] 16:08, 14 December 2010 (UTC)
:Also, specifying an algorithm is indeed better. I've changed the task to use the simple depth-first algorithm. --[[User:Abu|Abu]] 15:37, 14 December 2010 (UTC)
::Then change the task name to reflect the specified algorithm; as it is, it prevents exploration of others. --[[User:Short Circuit|Michael Mol]] 15:54, 14 December 2010 (UTC)
:::Right. I would suggest "Maze generation/Depth-first". However, I don't know how to change the name of a page :( --[[User:Abu|Abu]] 16:06, 14 December 2010 (UTC)
::::I moved the page to "Maze generation", but maybe we could start up several algorithm pages and we can add some nice text here and list them? If you don't want to do that I can just move it again. --[[User:Mwn3d|Mwn3d]] 16:44, 14 December 2010 (UTC)
:::::Thanks. Good idea. And maze traversal or solving are possible other subjects then. Let's see what other proposals come up. --[[User:Abu|Abu]] 16:58, 14 December 2010 (UTC)
:I am puzzled by the entry/exit mechanism.  Can the entry and exit be any place on the edge of the maze?  Or is the exit always from at of the two edges at the upper left corner and the entrance from either of the two edges at the lower right corner?  Or is the exit always the top edge of the upper left corner and the entrance the right edge of the lower right corner?  Or, perhaps, the entrance is always at the same spot (determined by the algorithm) but the exit can be anywhere?  --[[User:Rdm|Rdm]] 16:00, 14 December 2010 (UTC)
::For the purpose of maze generation, it doesn't matter where the entry and exit are. You can break two wholes into the outer wall after tha maze is done, and declare them as "entry" and "exit". --[[User:Abu|Abu]] 16:15, 14 December 2010 (UTC)
::: If you're thinking in terms of “entry and exit”, then the simplest method is to just place just two holes. Otherwise (when start and goal are within the maze) you can try to do things like finding the two places in the maze that have the longest path between them or, more easily, finding the place most distant from the initial seed point (easy to keep track of during building) and the place furthest from that (using a complete breadth-first search). However, such things require maze solution techniques, so it's easiest to just knock holes that are far apart; for a big enough maze, that's a reasonable approach and small mazes are trivial anyway. –[[User:Dkf|Donal Fellows]] 17:30, 18 December 2010 (UTC)


###  Criteria for being a real task? 

Is this draft task up to the level of being a real task yet? It has 4 implementations which all appear to be doing “the same thing” in a sufficient sense. If it's not yet worthy, what are the rules for graduation? –[[User:Dkf|Donal Fellows]] 18:21, 18 December 2010 (UTC)
: From my perspective, I like it. I look forward to seeing specializations and variations, too. When that eventually happens, "Maze generation" will probably be a term for a class of tasks, rather than a specific task itself. For now, I don't see a problem with it. --[[User:Short Circuit|Michael Mol]] 19:52, 18 December 2010 (UTC)
:I'd say we can change it to a real task. As a rule, I would propose that if a draft task didn't receive any objections for a certain time (say, one or two weeks), it is ready for graduation. Are there any other criteria? --[[User:Abu|Abu]] 07:01, 20 December 2010 (UTC)

:I think the main criteria now would be "who wouldn't want it out of draft and why"? If the task author and the writers of the several implementations haven't signalled any remianing problems then why not go ahead and change it? --[[User:Paddy3118|Paddy3118]] 08:10, 20 December 2010 (UTC)
:: As a general routine, that makes sense. The one remaining criteria for future tasks is that there may need to be multiple implementers. --[[User:Short Circuit|Michael Mol]] 14:45, 20 December 2010 (UTC)

== Python solution: incorrect ==

At least the sample output of the Python code is wrong.  There are three small blocks at the bottom that are sealed off, which shouldn't happen because of the recursive nature of the method specified by the task. --[[User:Ledrug|Ledrug]] 19:33, 30 June 2011 (UTC)
:This has also happened with the D solution, likely because it was a translation of the Python solution.  --[[User:Bnlott|Bnlott]] 19:40, 30 June 2011 (UTC)


 Can anyone explain to me how the python code works?
