+++
title = "Talk:Sorting algorithms/Cycle sort"
description = ""
date = 2014-06-14T01:36:44Z
aliases = []
[extra]
id = 17708
[taxonomies]
categories = []
tags = []
+++

==number of ''writes''==

<strike>
Am I missing something or is the sample algorithm (on the Wiki article '''Cycle sort''') incorrect in regards to the number of ''item writes'' (when placing items in the array)?

Whenever a swap is done, the ''write'' count is bumped by 1 (one), but in fact, two writes are done (for two items, agreeably it was done with a ''swap'' so it may appear that only one write was performed).   It appears it's counting swaps instead of writes. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:32, 13 June 2014 (UTC)

I only mention it as it appears one of the benefits of a ''cycle sort'' is the low number of writes being used during the cycle sort process. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:35, 13 June 2014 (UTC)

</strike>

-----

Nevermind, I see the Wikipedia algorithm was only counting writes to the original array. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:37, 13 June 2014 (UTC)

==4 REXXen==
Hi. Comparing the four REXX versions as a non-REXX programmer, the first seems neat and tidy in comparison to the other three that seem squashed in comparison. Are the other three really idiomatic and showing REXX off to advantage? Do you really want to show, for example, the single line while statements; or the code so squashed into the comments? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:47, 13 June 2014 (UTC)

: Adding liberal (some may say copious) comments to program statements puts constraints to squishing (or limiting) the code statement width somewhat, unless comments are not in-line, or if longer (wider) lines are used.   One of my concerns about most computer programs in Rosetta Code is a surprising lack of comments --- but I realize that my opinions about program comments can be construed as perjorative.   I'd rather add many comments than have a program with no or few comments, even if it looks not as neat. It certainly looks less tidy with more comments.   Most REXX programmers don't need to read those comments, but I assume a lot of non-REXX (or novice) programmers do.   Previously, I was thinking about making various program sources wider to accommodate longer comments and longer statements, but there was a (previous) concern about long statements in Rosetta Code, and indeed, some programs (not mine) were flagged as having statements that are too long (wide).   (I haven't re-visited those pages to see if the flags are still there or if the problem has been corrected, or if that concern is still held -- as I recall, the statements in question were multiple IF statements and had no following comments attached on the same line.)   So I normally use a width of 74 characters for source statements.   And yes, it is idiomatic REXX (although not everyone uses that style), and any program can be made neater (tidy?) by having no comments, and by introducing more statements that are normally shown on one line (''if x=y then ↓ x=0'' instead of ''if x=y then x=0''), using superfluous DO groups and dead code, etc;   it's a matter of style (although superfluous DO groups and dead code is where I draw the line).   Same thing with indentation(s) with IF then/else, SELECT, and DO loops (I'm a firm believer in not using Egyptian indentations).   I prefer to view programs or functions/subroutines on a single screen (if possible) instead of scrolling.   I have yet to find a Classic REXX (interpreter) that executes version 1 (but of course, I haven't tested them all, but all of the REXXes I have installed on my PC, none do);   to that end, I wrote a Classic REXX solution that followed the Wiki algorithm.   I also added refinements to the program, and rather than making the program to hard to follow after the acute changes, I added several variants to make the changes easier to recognize from the original. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:01, 14 June 2014 (UTC)

::Thanks Gerard for the reply. I guess it is a matter of style, and knowing that you have considered things then that's fine. 
::P.s. maybe you could ask for the REXX version that works with version 1 if you interested in confirming that it works on something? Or maybe not. Only if you are interested I guess. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 00:32, 14 June 2014 (UTC)

-----

::: No thanks, I've beat that dead horse before.   I assume the aforementioned REXX program works for ooRexx   (since it uses a construct that is only valid in ooRexx);   although it was my version that was translated to ooRexx, not version 1. 

[Note that ooRexx and NetRexx are considered different languages than REXX, but it is thought that it might be possible to (re-)write most REXX programs so that they will execute under ooRexx and NetRexx if certain restrictions are followed --- this is an on-going   ··· discussion   about this very matter --- does a programmer restrict their programming of (Classic) REXX so as to also execute under a different language such as object-oriented REXX?] 

I'm a firm (and maybe overly religiously concerned) believer that any computer program solution posted under the (Classic) REXX language actually be tested/executed under a (Classic) REXX interpreter   --- well, this would extend to all Rosetta Code languages.   One would think this would be axiomatic on Rosetta Code.

Elsewhere, I wrote about a contest during my college days (cough, cough, Eisenhower was still alive) to write a (singular) program to compile and execute correctly under COBOL, FORTRAN, ASSEMBLER (IBM), ''and'' PL/I.   --- Yeah, yeah, yeah, back then, the languages were all capitalized and most of us used candles, quills, and parchment.   I view that endeavor that same as (Classic) REXX and ooRexx (and NetRexx). 

I know of several REXX examples that have been entered on Rosetta Code that don't work for a Classic REXX, but rather than hit that beehive with a stick, I just enter my own REXX program version.   Meanwhile, back at the ranch ... whenever I find one of my REXX computer programs that don't work for all the REXXes that I have installed, I re-write the REXX program so that it does execute correctly under all REXXes (that I have), unless otherwise noted as being specific to a particular REXX interpreter. 

The various (Classic) REXX interpreters are very fortunate in that there are few differences between them, the main differences are those that have features or BIFs added. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:34, 14 June 2014 (UTC)

-----
