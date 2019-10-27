+++
title = "Rosetta Code:Village Pump/Whoa! 10000 examples!"
description = ""
date = 2018-09-14T18:35:24Z
aliases = []
[extra]
id = 5389
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=10000 examples
|summary=Rosetta Code reaches 10000 entries
}}
According to both the Oz and the Python version of [[Count programming examples]], we reached 10000 entries! Way to go everyone!

(The 10000th entry is quite prosaic: [[Loop over multiple arrays simultaneously]] in C++.) -- [[User:Wmeyer|Wmeyer]] 09:52, 28 January 2010 (UTC)

==total number of Rosetta Code entries==
In the Rosetta Code task:   '''Rank languages by popularity''',   near the top of the output of the REXX entry, the total number of Rosetta Code solutions (computer programming examples) entered can be viewed here   ──►   [[RC_POP.OUT]]. 

The output for the REXX program is usually updated every month. 


<strike>
As of February 27, 2017,   there are over   '''53,000'''   computer programming examples (entries).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:37, 27 February 2017 (UTC)

As of November 14, 2017,   there are over   '''57,068'''   computer programming examples (entries).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:18, 14 November 2017 (UTC)
</strike>



As of August 11, 2018,   there are over   '''60, 282'''   computer programming examples (entries).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:30, 13 August 2018 (UTC)

== [[wp:Lies,_damned_lies,_and_statistics|Lies, damned lies and statistics]] ==
It is almost impossible to get an accurate algoritmically determined count of programming examples. The number cited above ('''60,282 as of 08/13/2018''') is inflated by including all categories related to a programming language, not just programming examples.

The numbers on the [[Rosetta_Code/Rank_languages_by_popularity/Full_list]] are obtained by directly querying the site "For each programming language, how many programming examples are there?" ('''59,366 as of 09/11/2018'''), so that is an "accurate" count insofar as it is the one that the site agrees with, but that suffers from it own inaccuracies.

There are several language groups which are counted as separate languages but only have a single entry "credited" to both languages. [[Icon]] / [[Unicon]] is perhaps the clearest example. Each has 587 (currently) tasks listed under their respective language page, but each has only a single example under each task credited to both. [[Mathematica]] / [[Wolfram_language]], various flavors of Basic and other family groups of languages also have some overlap, so some examples are counted twice, inflating the actual number.

Ok, If I look at the totals from [[Rosetta_Code/Count_examples/Full_list]], how about that? Well... yes and no. The code generating that page actually visits every task page and manually counts the number of task examples actually on each page, and is nominally the "most" accurate, ('''58,101 as of 09/11/2018''') but even it totally glosses over the fact that many languages have multiple solution examples for a particular task. This count only checks that a programming language '''has an entry''' on the task page.

''Technically'' it could be argued that each of the multiple solutions should count as a programming example because ''technically'' they are. Trying to accurately count them would be extremely challenging though. Things inside a <nowiki>
```whatever>
```
</nowiki
 block may be a complete example or may just be a fragment of a larger program, or may be something else entirely. Due to the variability in how each task author has formatted their various entries it is nearly impossible to (accurately) programmatically count them. 

What does all of this mean? Not much, I suppose. It is just something I have pondered on a bit and decided to share my observations.--[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 17:43, 14 September 2018 (UTC)

:59K +/-2% or so? [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:20, 14 September 2018 (UTC)
