+++
title = "Talk:Chess player"
description = ""
date = 2019-07-29T14:47:28Z
aliases = []
[extra]
id = 11660
[taxonomies]
categories = []
tags = []
+++

== Task size ==

This draft task, while fairly well defined, feels daunting. Is it possible to at least split it into pieces (e.g., framework to allow two players to play chess, and a robot implementation of a player)? –[[User:Dkf|Donal Fellows]] 10:55, 24 April 2012 (UTC)
: My intention for this task was only the second part. A special chess front end is not necessary, just the input and output of moves (it is just an add-on that the PicoLisp solution outputs a simple ASCII board display). --[[User:Abu|Abu]] 12:20, 24 April 2012 (UTC)

I didn't know that single-word names should be avoided. Then perhaps a better name would have been "Chess engine"? This is how it seems to be usually called. --[[User:Abu|Abu]] 12:20, 24 April 2012 (UTC)
:: On the other hand, "Chess engine" would sound ''really'' daunting ;-) --[[User:Abu|Abu]] 14:34, 24 April 2012 (UTC)
::: The key is that there are many other chess-related tasks possible (e.g., print the board state using UNICODE symbols, which would be at the simple end of tasks). A single-word name is so easily confusing, so we aim for clarity. (It's also easy to rename pages.) –[[User:Dkf|Donal Fellows]] 08:13, 25 April 2012 (UTC)

I think this task is too large, having reviewed and written several small chess programs in various languages.  How about some smaller pieces:  alpha-beta search,  move generation,  board representation, xboard or UCI protocol handler,  EPD parser.  Note that there are entire wikis devoted to just this task  (http://chessprogramming.wikispaces.com).
: I don't think so. The PicoLisp solution has 447 lines (without the white space). There are tasks in RosettaCode with larger solutions. I feel it would be difficult to separate the pieces, as they strongly depend on each other. --[[User:Abu|Abu]] 18:04, 24 April 2012 (UTC)
:: I think some languages have solutions (and output traces!) that are far too long; I like tasks to be such that at least the majority of solutions will be able to fit on a single screen, so you can see the whole of them at once. (Can't be done with some languages; for example, [[C]] is usually fairly verbose.) Whilst still being idiomatic; that's ''important''. That said, a player (even if it only chooses randomly from the legal moves) would be a reasonable start. That would then be a start of what any more sophisticated player would need. –[[User:Dkf|Donal Fellows]] 14:04, 25 April 2012 (UTC)

I would love to see folks attempt a modern chess engine in other languages though.  My recent survey found that all the top engines were in C/C++ (rated 3000-3300), with the [[Delphi]] engine Booot at 2935, [[Java]] engine Cuckoo at 2675, and [[C sharp|C#]] engine Pupsi at 2610.  Engines in other languages were only of amateur demo quality, not even worth mentioning. --[[User:IanOsgood|IanOsgood]] 17:21, 24 April 2012 (UTC)
: I suspect that a modern chess engine capable of playing at those sorts of levels would be out of the scope of RC. We're focused on idiomatic and beautiful examples showing by example how to program with many languages. A high quality player is likely to be sufficiently complex ''independent of language'' as to be hard for most programmers to work through. That there are other sites devoted to just this task is a good indication that it is out of our scope! –[[User:Dkf|Donal Fellows]] 13:50, 25 April 2012 (UTC)


### End it?

It has been two years and there is still only one solution Abu. I think the RC community thinks this is too large a task. I vote that this be deleted. Maybe the suggestion of having a smaller chess-related task should be followed? (Something where a succinct algorithm can be shown)? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:39, 16 April 2014 (UTC)

I think deletion is a bit extreme. Certainly this task is enormous. I vote that rather than delete the task, simplify it to "checkmate with a rook and a king versus king" with a fixed starting position.  Or maybe "given a FEN string, generate and print legal moves" [[User:Toadofsky|Toadofsky]] ([[User talk:Toadofsky|talk]]) 17:44, 16 April 2015 (UTC)

=== Sub-tasks ===
I have created some sub-tasks, and specified 'basic' and 'advanced' options for each of them. This should hopefully spur some people to implement some of them as I think they are quite manageable (at the basic level anyway). I might add implementations later. Perhaps the tasks could be further broken down?  Suggestions welcome. [[User:TobyK|TobyK]] ([[User talk:TobyK|talk]]) 19:03, 18 May 2015 (UTC)

: I shouldn't mention this draft task in your subtasks but make them smaller and self-sufficient. It does look as if this will never leave draft status and will likely be deleted. The Python examples fit a task description of "Name a chess library for your language" which seems more workable than this current task for RC, but still wouldn't make a good RC task in my opinion. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:03, 18 May 2015 (UTC)

: Clearly the sub-tasks have not helped and should be deleted. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 11:00, 19 July 2019 (UTC)

::I'd second that.

::It's more than 4 years now since the sub-tasks were created and no solutions have been posted.

::I doubt in any case whether anyone trying to write something from scratch would get much beyond the first sub-task which is hard enough to do in a reasonable time for RC purposes. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 14:47, 29 July 2019 (UTC)
