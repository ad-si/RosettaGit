+++
title = "Talk:Sudoku/REXX"
description = ""
date = 2014-10-03T23:39:48Z
aliases = []
[extra]
id = 12869
[taxonomies]
categories = []
tags = []
+++

==history of the REXX program==

This REXX program started as a simple program just to supply hints to difficult sudoku puzzles.

As it evolved, it provided more types (kinds) of hints, eventually evolving into solving the puzzle.

One of the design criteria was to present the sudoku puzzle in a conventional manner as possible, and

to display it as large as possible while still fitting on a ("standard") terminal screen (a "DOS prompt" windows). 


 -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:54, 31 January 2013 (UTC)

==Move?==
Hi Gerald. Could the page be moved to be called "Sudoku.rex" to better fit with the rule for task titles?
: [No '''L''' in Gerard.] -- [[User:Gerard Schildberger|Gerard Schildberger]] 15:46, 31 January 2013 (UTC)
:(And similar for the other pages)? --[[User:Paddy3118|Paddy3118]] 07:29, 31 January 2013 (UTC)

: It probably ought to be a child of [[Sudoku]], which is the only page that refers to it at the moment. –[[User:Dkf|Donal Fellows]] 13:38, 31 January 2013 (UTC)

:: I named it (and a few others) by what I named it on my system, and I have an update system in place for updating/maintenance/verification/backups/etc (we're talking about more than just a few files);   also "uploaded" were similarly named REXX files (subroutines) which are called (invoked) by various REXX programs which are also "'''$'''"-named routines (programs).    [The reason why they almost all start with a dollar sign is a story that dates back to the very early 1970's VM/CMS if anyone wants to hear about it.]   The few REXX programs that don't having that naming convention are the few PC/REXX and Personal REXX extended BIFs (built-in functions) that I wrote for other REXXes that didn't support them: LINESIZE, SCRSIZE, DOSCD, DOSDISK, DOSDRIVE, DOSISDIR, SOUND, CHANGESTR, JUSTIFY, DELAY, and some others.]   It makes it much easier for me (and anybody who may take on that job later) to maintain and update those files if they have the same name everywhere.   Also, the HELp files mention the program (and how to invoke it) by name.   Ditto for the flow (FLOw) and sample (SAMple) documentation files --- which I haven't placed on Rosetta Code yet (and may not).    I've been ''thinking'' (with reservations) about also "uploading" additional (two) help-type files:   one is for a general flow-type visual diagram (each 21 lines only, one is included below, just for show-n-tell),   along with a sample-use documentation   (i.e.,   how to use/invoke the program with various options and whatnot), but I'll wait on that as I don't want to start down a road and then have to rename them all, update each of those pieces to refer to their changed names (or just remove all of 'em).   I don't want to have to maintain yet another time-consuming off-site system that has a different naming system and/or requirements.   I realize that Rosetta Code may not be the proper forum for large programs (its all relative, of course, of course);   programs that do much more that only address tasks with few (or limited) requirements.   '''$SPELL#'''   is one such REXX program, it goes beyond what is required, but then, I wrote that program long before Rosetta Code was even thought about, indeed, even before the internet (as I know it).   I'd like to do that with a few others (REXX programs), such as the Morse code solution, drawing a clock, and a couple of others.   A lot of the subroutines (which are programs in the truest sense of the word) and are very robust and more general-purpose than what is needed to solve the various task's requirements, but as I mentioned elsewhere, I'd rather show the whole program than ripping out the guts of a program (and testing it) and include that anemic version on Rosetta Code.   It would be much easier to just include the whole shebang in the first place, preserving the robustness and including other features and options, which, for the most part, would be too much to ask as general requirements, not to mention just too difficult.   Also, I don't want a special version of the various HELp (documentation) files just for Rosetta Code, updating them is a chore by itself; I wish I could upload the files directly instead of cut-n-paste (and doing that for a larger file is a pain in the neckhole and prone to mishaps).   A thought about ''child'' storage of programs:   I understand why the sudoku REXX program would/should/could/maybe be renamed to reflect the task name, but what about the various subroutines that it (and other REXX programs) invoke?   In addition, the subroutines are used elsewhere, so by themselves, they aren't children of any particular Rosetta Code task solving examples.   It's a lot easier to have other people take on that chore, but it's becoming a chore.   I'd rather spend time on solving tasks than making more work for myself. --  [[User:Gerard Schildberger|Gerard Schildberger]] 15:44, 31 January 2013 (UTC) 

::: From our perspective, the main issue is just that it appears to be something that is mainly of interest within the context of, ''first'', solving a sudoku, and ''second'', using REXX. There's no problem at all with putting such longer solutions on their own page (and other languages do this too), but (we believe) it should be turned into a child page of the [[Sudoku]] task rather than being an apparent-orphan. Where there's a general larger principle of REXX being demonstrated — there might or might not be, I don't know REXX at all so I'm not best qualified to comment on that — then that could be done as a child page of the [[REXX]] page (though I think it can't be a child of the Category page for ugly-technical reasons). There's also the possibility that any such general principles might lead to their own tasks if they mean something in other languages too, but that's something for discussion in the appropriate [[Rosetta Code:Village Pump/Suggest a programming task|Village Pump pages]]. After all, that's the proper place for that discussion. –[[User:Dkf|Donal Fellows]] 22:59, 31 January 2013 (UTC)

:::: I'm not quite fully appreciating what you mean by a ''child-page''.   In the REXX example under '''Number names''' task, there is a thingy that is (or looks like) '''&lt; Number names '''       ----- but I can't see how that is done, another one of those things that remain hidden to me (if I can't view it, I can't learn how to use it), but it's a good way to un-orphan it --- er, I mean, show who the parent is. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:43, 31 January 2013 (UTC)

:::: Since the REXX programming example is linked from the parent-page   '''sudoku''', doesn't that linkage make this REXX programming (example) page a child-page of   '''sudoku'''?   This all started when I asked how to (of all things) upload a (programming/example) page rather than cut-and-paste it into the "main" pages.   Nothing was said about parent/child pages, which is just as well, as I'm beginning to believe that I know less and less about the overall structure of this animal. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:27, 1 February 2013 (UTC)

::::: Gerard, you may wish to look at [http://rosettacode.org/wiki/Knapsack_problem Knapsack_problem]. The four tasks on this page are children of [http://rosettacode.org/wiki/Knapsack_problem Knapsack_problem]. To do this you enter "Knapsack problem/Bounded" in the create page dialog. Similarly [http://rosettacode.org/wiki/Knapsack_problem/Bounded/Mathprog Mathprog solution] is a child of [http://rosettacode.org/wiki/Knapsack_problem/Bounded  Knapsack_problem/Bounded] and was created by entering "Knapsack_problem/Bounded/Mathprog" in the create page dialog.--[[User:Nigel Galloway|Nigel Galloway]] 12:32, 2 February 2013 (UTC)

::: Also, looking below I'd note that such diagrams don't work too well here; the fonts across different browsers are just too variable, alas. An image of how it should look, as grabbed on your system (and trimming any extra stuff like window borders) actually works much better. –[[User:Dkf|Donal Fellows]] 22:59, 31 January 2013 (UTC)

:::: Are you saying that it won't be rendered true in the same font as I cut-and-pasted it?   Is it dependent upon the type of font that you have on your (viewing) computer?   That would make it very exasperating to show anything in that case (especially boxes), I had assumed that if I used a fixed font, it would show up as a fixed font for whoever viewed it. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:43, 31 January 2013 (UTC) 

An example of the FLOw documentation   ('''$T.FLO''')   for the   '''$T'''   REXX program:

```txt

     ┌───────────────┐       ╔═══════════╗
     │ $H  for ?opts ├────◄──╢           ║
     └───────────────┘       ║    $T     ║
                             ║           ║                           ┌────────┐
                             ║           ║     ┌─────────────────┐   │ $T  for│
                             ║     errors╟───►─┤ $ERR for err msg├─►─┤  color │
  ┌──────────────────┐       ║           ║     └─────────────────┘   │   msgs │
  │ $MKDIR  (if DOS) ├────◄──╢.F=yyy     ║                           └────────┘
  └───────────────┬──┘       ║           ║
                  └────►─────╢           ║      ┌─────────┐
                             ║      {all}╟──►───┤ SCRSIZE │
                             ║           ║      └──┬──────┘
         ┌────────┐          ║           ╟─────◄───┘
         │ $SCALE ├───────◄──╢.RULER=nnn ║
         └──────┬─┘          ║.SCALE=nnn ║
                └────►───────╢           ║
                             ║           ║         ┌────────┐
                             ║ .BLOCK=yyy╟──►──────┤ $BLOCK │
                             ║           ║         └─┬──────┘
                             ║           ╟──────◄────┘
                             ╚═══════════╝

```

-- [[User:Gerard Schildberger|Gerard Schildberger]] 15:44, 31 January 2013 (UTC)

== Moved and consolidated ==
I will delete stray pages soon. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:39, 3 October 2014 (UTC)
