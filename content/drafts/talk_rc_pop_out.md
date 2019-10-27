+++
title = "Talk:RC POP.OUT"
description = ""
date = 2019-05-06T05:05:51Z
aliases = []
[extra]
id = 16821
[taxonomies]
categories = []
tags = []
+++

==language popularity==

The REXX (program) that generates the output only lists those languages that have an entry.

Languages with no entry (no solutions) are not listed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:01, 5 December 2013 (UTC)


Why was this Rosetta Code task definition/requirements changed?

The following text was deleted:

```txt

Sort most popular programming languages based in number of members in Rosetta Code categories.
(from http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)

```


This change/deletion changes the very definition of what's being reported. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:55, 28 May 2015 (UTC)

:[[User:Gerard Schildberger|Gerard Schildberger]], would it make sense to include the zero-solution languages? That way we can compare with solutions from other languages and compare where the differences are. For example my current Awk solution is only giving 484 languages with > 0 solutions, but 574 total (the difference being the 0 solution languages). I'd like to see what your zero-solution languages are to help debug the problem. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 19:25, 27 May 2015 (UTC)

::: The REXX language has no API for looking at web pages, so it wouldn't be feasible on report on zero-entry computer language entries.   I had only written/entered a REXX solution because, at the time, none of the entries that I tried worked (I tried just a few of the entries that I had compilers for), or reported incorrect rankings, or didn't report on some of the top entries, or reported on categories that weren't computer programming languages.   That left me looking at (various) entry's results (output), none of which displayed enough depth to see where the REXX computer language was ranked   (among other languages).   So I wrote a REXX program to display the ranking of   ''all''   the computer program languages, along with correctly showing tied entries.   Since the REXX entry was the only entry to show   ''all''   the languages, I keep it updated (about) every month or so (on a separate web page). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:31, 28 May 2015 (UTC)

:: (See my comments at the copied/quoted part of a talk page near the bottom of this talk secion.)   The REXX programming language entry shows the number of members (as per the Rosetta Code task), not the number of solutions. 


Hello again. I looked into this further and it appears the problem may be with the REXX code? For example [[:Category:80386 Assembly]] has 1 page, but REXX is reporting 4 pages. Probably the most reliable solution is to use the API to report how many pages are available. This shows the first 500 languages with corresponding page totals:
::rosettacode.org/mw/api.php?action=query&generator=categorymembers&gcmtitle=Category:Programming%20Languages&gcmlimit=500&gcmcontinue=&prop=categoryinfo&format=txt
Then check the gcmcontinue code at the top to cycle through each additional 500 results until complete. The Awk solution does this. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 20:59, 27 May 2015 (UTC)

: There is no problem with the REXX code (as per the original Rosetta Code task's requirements).   The REXX program uses the   ''categories''   page, not the   ''languages''   page.   The REXX language entry uses the   ''languages''   page solely as a verification file to validate if a category entry is indeed a computer programming language entry.   Since the REXX entry uses the   ''categories''   page, there are no entries in that page that have zero entries (for a computer programming language).   One just shouldn't change the task requirements and/or definitions this late in the game, I should think. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:14, 28 May 2015 (UTC)





: As mentioned in the talk page for the Rosetta Code task   ''Rank languages by popularity'',  


{The text below was copied wholesale from the talk section '''==wanted: a complete list=='''   (at the very end of that section):}

:::: <big> ----- start of copied (quoted) material. ----- </big>

-----

:According to [http://rosettacode.org/wiki/Category:A%2B its task page] A+ has no tasks implemented. It seems as if a language with no tasks implemented is treated as if it has three
:: rank: 441         (3 entries)  A+
:--[[User:Nigel Galloway|Nigel Galloway]] 14:51, 26 January 2013 (UTC)

:: Using the number of members in a category is biased because most language categories have three subcategories ("X examples needing attention", "X Implementations" and "X User") which are also included in the count. Many (all?) languages that are listed with 3 members (like [[A+]] or [[B]]) in the ranking actually don't have any task implemented. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 13:18, 12 March 2014 (UTC)

-----

I took the task's requirements quite literally:


''Sort most popular programming languages based in number of '''members''' in Rosetta Code categories''

 (from http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)


(The bold font was added by me.)   Note that it didn't say   '''implementations''',   but   '''members'''.

I think that's what most people (most likely) thought that's what was wanted, but there ya have it. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:45, 26 January 2013 (UTC)

:::: <big> ---- end of copied (quoted) material. ---- </big>

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:55, 28 May 2015 (UTC)


[[User:Gerard Schildberger]], the list is "popularity", per the title. The criteria may have been better worded, as always, but what's expected is a measure of "popularity". As the 2013 discussion points out, the subcategory names shouldn't be included as they don't measure popularity. What the person who wrote the criteria meant when they said "members" was obviously "implementations". Otherwise, what is this list of? Not popularity. There is a contradiction, the title says one thing and the criteria another, so we have to judge what was meant, "popularity" of languages was clearly the intention. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 01:42, 28 May 2015 (UTC)

:::: Please don't do major editing to your text after I have replied/responded to it (the old text),   now, my response(s) is/are to non-existent prose. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:37, 28 May 2015 (UTC) 

:::::I don't know what you're talking about. Can you use the history tab and post diffs to support your accusation?   -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 04:13, 28 May 2015 (UTC)

:::::: I didn't mean it as an accusation, but an observation.   In any case ...

:::::: The original text was (I added line breaks for the PRE html tag):

```txt

[[User:Gerard Schildberger]], the list is one on popularity, per 
the title. The criteria may have been better worded, as always, 
but what's expected is popularity. As the 2013 discussion points 
out, the categories shouldn't be included as they don't measure 
popularity. 
-- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] 
([[User talk:3havj7t3nps8z8wij3g9|talk]]) 01:42, 28 May 2015 (UTC)


```


:::::: The updated text is (I added line breaks for the PRE html tag):

```txt

[[User:Gerard Schildberger]], the list is "popularity", per the 
title. The criteria may have been better worded, as always, but 
what's expected is a measure of "popularity". As the 2013 discussion 
points out, the subcategory names shouldn't be included as they 
don't measure popularity. What the person who wrote the criteria 
meant when they said "members" was obviously "implementations". 
Otherwise, what is this list of? Not popularity. There is a 
contradiction, the title says one thing and the criteria another, 
so we have to judge what was meant, "popularity" of languages was 
clearly the intention. 
-- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] 
([[User talk:3havj7t3nps8z8wij3g9|talk]]) 01:42, 28 May 2015 (UTC)

```

:::::: This is via   ''cut-n-paste''   from the observing differences from the   ''View history''   tab.   Please note that I'm not disagreeing with the veracity of your statements/text, but that the Rosetta Code task wording (task requirements) was changed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:58, 28 May 2015 (UTC)

::::::: (replying to the newer text):   I read the requirement:     ''Sort most popular programming languages based in number of members in Rosetta Code categories ...''     exactly as stated.   I saw no need to judge that requirement and conclude that he meant anything different than other what he wrote.   The word '''popularity''' was clearly defined within the same sentence, and even stated where to get the needed data. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:58, 28 May 2015 (UTC)

::::::::Gerard if you look at the history tab I did NOT change my post AFTER you posted. You said "Please don't do major editing to your text '''after''' I have replied/responded to it". That did not happen. I'm free to edit my post before anyone else has replied to it. The time stamp between my first and second edit was all of 5 minutes, I was still working on it! -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 14:11, 28 May 2015 (UTC)

: Even so, I took the ('''original''') task's description/requirements from the task, not it's discussion/talk page, or for that matter, some expectation.   I would have expected that the task's requirements and/or description would be discussed first (at least by the original author of the Rosetta Code task) before it was changed.   The task still states (and implies) that popularity is based on the number of members in (from) the category page, and I used (for the REXX version) the   ''languages''   page to filter out the non-language entries from the category page.   That may not be what some expectations are, but that is what I used when I entered the REXX language entry.   I don't use the ''title'' for a task's requirement, only the task's (requirements) text. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:14, 28 May 2015 (UTC) 


::Yeah that's fine and understood. It's not your fault. Many people have seen the flaw in the task description section and programmed according to "popularity" and not "members". -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 04:13, 28 May 2015 (UTC)

::: I never saw it as a flaw in the task description (requirements), but took it at face value.   The Rosetta Code task was already a task, not a draft, so I didn't bother to contest the wording of the task.   It was this (older, original) requirement that I used when coding the REXX computer language entry.   If the original author wanted language entries, he should've defined what "popularity" is with some other wording instead of using the word "members".   But the word "members" was used, and re-wording that '''is''' a change. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:58, 28 May 2015 (UTC)

<strike>

==limit=5000==
The jq example uses limit=5000:

```sh
'http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000'
```

This (together with the single-page query Category:Programming_Languages) seems like the simplest approach.  Is there a problem with that?
--[[User:Peak|Peak]] ([[User talk:Peak|talk]]) 21:09, 27 May 2015 (UTC)

-----

Please note that all the above sections are the '''talk/discussion''' section for the (only) '''output''' section of the REXX language entry for the Rosetta Code task:   Rosetta Code (computer programming) task:     ''Rank_languages_by_popularity''.   You may want to transfer this last particular section to the '''talk/discussion''' section of that Rosetta Code task. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:56, 27 May 2015 (UTC)

</strike>

:Moved to [[Talk:Rosetta_Code/Rank_languages_by_popularity#limit.3D5000]] -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 00:02, 28 May 2015 (UTC)

: "Struck"     <big> <nowiki> <strike> ··· </strike> </nowiki> </big>     (after being moved)   by:    -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:25, 28 May 2015 (UTC)

----
==Kotlin?==

So.. has Kotlin achieved a higher plane of existence? It seems to have disappeared completely from the rankings despite it having the most examples. ----[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 13:04, 15 May 2018 (UTC)

::Hehe, looks like reaching a 1,000 tasks is a 'black hole' in the RC POP.OUT. Expect to see other languages disappear as they reach the 'event horizon'. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 14:21, 15 May 2018 (UTC)

::: Yes, I identified the problem with the REXX program logic, it was looking at the   ''members''   follow-on keyword, and   ''if''   it wasn't numeric, it was ignored.   A string with a comma in it isn't considered numeric in the REXX language, so I changed the program to remove any commas before checking for a numeric string.   I suspect, as you do, that other (most?) computer programming entries will also fail.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:14, 15 May 2018 (UTC)

:: I wasn't sure whether the problem was the extra digit or the comma but I knew you'd soon sort it out :) As you say, there are probably not many languages which can identify a string of digits with thousand separators as being numeric without further ado. Kotlin certainly can't. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 18:37, 15 May 2018 (UTC)
==Mathematica vs Wolfram Language==
(Changed a word in the talk section title from '''Mathmatica''' to '''Mathematica'''.)     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:38, 10 February 2019 (UTC)

What difference between:

```txt

rank:  22               (743 entries)  Mathematica
rank:  80               (257 entries)  Wolfram Language

```

is RC.POP.OUT making? Is the 24 game included in Mathmatica? --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:47, 10 February 2019 (UTC)



-----



If you meant   '''Mathematica''',   then yes, the   '''24 game'''   is included in the count for   '''Mathematica'''.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:09, 10 February 2019 (UTC)




'''RC_POP.REX'''   is the name of the REXX program. 

The (or my) REXX program isn't making any decision.   It uses two files for input:
::*   Categories     {http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)}
::*   Category:Programming_Languages     {http://www.rosettacode.org/wiki/Category:Programming_Languages}


It scans the 1<sup>st</sup> file for (any) categories, and each category is checked against the 2<sup>nd</sup> file to verify that it is a computer programming language known to Rosetta Code.   If it passes the verification,   then it's a computer language that is used on Rosetta Code,   and the number of entries (programming examples) is taken from the   '''members'''   keyword.   No distinction or decision was made on my part or the REXX program's part between the aforementioned computer programming languages. 

I suspect that various peoples used one name instead of the other (interchangeably). 

If someone with god-like powers would make an executive decision to use one over the other, I suppose someone would write a script to change all the uses of one computer programming language into the other, or I could special case this specific example   (for the REXX programming entry)   into just simply combining those two programming entries into one,   much like the way it currently handles different spellings of a small number of computer programming languages that are spelled differently, or use different glyphs the express the small programming language.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:57, 10 February 2019 (UTC)

This decision would be similar of using   '''PL/I'''   instead of   '''PL/1''';   Each computer programming entry for   '''PL/1'''   was changed "by hand" to   '''PL/I'''.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:29, 10 February 2019 (UTC)

I prefer the global script change method   (or by hand),   as this will help consolidate the computer programming language name and would have less confusion to those that think that the two programming languages are distinct entities.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:03, 10 February 2019 (UTC)

I just noticed that some Rosetta Code tasks that have   '''Mathematica'''   also have (in conjunction)   '''Wolfram Language'''   

(in the same   <big> <nowiki> 
## Mathematica / Wolfram Language
 </nowiki> </big>   header). 

In that case, each specified language would have it's '''members''' count to be included, so that it appears that in this case, it can appear as double-counting   (in a global total sense).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:59, 10 February 2019 (UTC)

Also, as a by-product of having two computer programming languages specified in the 

  <big> <nowiki> 
## Mathematica / Wolfram Language
 </nowiki> </big>   header,   it would appear like there was an entry for each of the two languages specified, thus my comment about double counting.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:04, 11 February 2019 (UTC)

:See my musing on this subject (from last September) here: [http://rosettacode.org/wiki/Rosetta_Code:Village_Pump/Whoa!_10000_examples!#Lies.2C_damned_lies_and_statistics Lies, damned lies, and statistics]  --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 00:23, 11 February 2019 (UTC)


== Category vs. solution ==

In the latest version of the page you added the line:
<blockquote>Only computer programming languages that have at least one programming solution are listed.</blockquote>

but that is not the case. If it is, please point out a link to '''any''' programming solution for '''any''' of the following:

*[[Script Basic]]
*[[.QL]]
*[[8 1/2]]
*[[A+]]
*[[Agda2]]
*[[ALGOL]]
*[[Alice ML]]
*[[ANT]]
*[[Application Master]]
*[[ASP]]
*[[ASP.Net]]
*[[AspectC++]]
*[[Axum]]
*[[B]]
*[[Beta]]
*[[C0H]]
*[[Caml]]
*[[CB80]]
*[[Cecil]]
*[[Cilk]]
*[[Cilk++]]
*[[Datalog]]
*[[Diesel]]
*[[Elan]]
*[[Euler]]
*[[FAUST]]
*[[FeatureC++]]
*[[FPI]]
*[[FreeMat]]
*[[Goo]]
*[[Jabaco]]
*[[KeyList Databasing]]
*[[L.in.oleum]]
*[[Lotus 123 Macro Scripting]]
*[[Lout]]
*[[M680x0]]
*[[ME10 macro]]
*[[MGS]]
*[[Mirelle]]
*[[Mython]]
*[[Nice]]
*[[NQP]]
*[[OpenC++]]
*[[Pentium Assembly]]
*[[PLUS]]
*[[PLZ/SYS]]
*[[PPC Assembly]]
*[[PPL]]
*[[QuakeC]]
*[[Refal]]
*[[RLSL]]
*[[RTSL]]
*[[Script3D]]
*[[Superbase BASIC]]
*[[TAL]]
*[[TeLa]]
*[[Thistle]]
*[[UScript]]
*[[UserRPL]]
*[[VRML]]
*[[WML]]
*[[Xbase]]
*[[CHR]]
*[[MAPPER]]

RC POP.OUT is a count of ''categories'' '''not''' ''programming solutions''.

BTW, there are 639 languages with at least one solution on the site. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 00:16, 6 May 2019 (UTC)

: Sorry for the misquote.   I should've mentioned ''members'', not programming solutions.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:04, 6 May 2019 (UTC)
