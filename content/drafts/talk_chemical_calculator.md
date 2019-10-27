+++
title = "Talk:Chemical Calculator"
description = ""
date = 2019-09-01T16:53:50Z
aliases = []
[extra]
id = 22231
[taxonomies]
categories = []
tags = []
+++

== the task description needs to be complete == 
The task description needs to be complete, without needing chemistry knowledge.  Describe how to perform the calculation; input format; data tables etc. Aim the task at those who know how to program but '''don't''' know chemistry, for example. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:01, 18 March 2019 (UTC)

Might be an idea to specify an element symbol consists of one upper-case letter followed by 0, 1 or 2 lower-case letters, so CH4 is uniquely parsed as 1 atom of element C followed by 4 atoms of element H.
(The symbols with two lower case letters are for systematic names of as yet unnamed elements, e.g. Ubn would be the symbol of element 120).

Also (sorry to nit-pick but...) Carbon 12 has exactly 6 protons, 6 neutrons and 6 electrons. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 21:03, 18 March 2019 (UTC)

I leave three letter names for refactoring later when these substances have been found
Thanks for telling me about C-12 and the neutrons. I've update the page.
--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 22:29, 18 March 2019 (UTC)

: Some ''have been'' found, and those elements need approval to be (re-)named after confirmation (of existence) has been made by (other?) scientists/chemists/physicists/powers-that-be.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:04, 19 March 2019 (UTC)

Gerard, it seems that you think three letter names are unnecessary. Anyway, it's just a matter of changing an '''if''' to a '''while''' in the parse function.
--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 02:58, 19 March 2019 (UTC)

: I have no idea where you got such a wrong idea.   Where or how did you come to that conclusion?   Three-letter names for elements are necessary for various reasons.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:35, 19 March 2019 (UTC)

Gerard, I misunderstood you, and have updated Python to allow for three letter names.
--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 10:19, 19 March 2019 (UTC)

== What to do? ==
All examples need to be computing the same set of results for comparison (adjusting for floating point issues).
You need to add to the task description defining what those are.

(P.S. I do like this task though :-)

 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:48, 19 March 2019 (UTC)

Paddy, what do you think about using three decimals in the result?
--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 23:13, 19 March 2019 (UTC)

Should I keep the really bloated Python version as a reminder of the progress being done during the process? I'm really happy my understanding of RegEx is better now thanks to RC.--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 23:13, 19 March 2019 (UTC)

== Mass number and atomic weight ==

There seems to be some confusion here. The mass number is the total number of nucleons in a nucleus. The standard atomic weight is the average observed weight, and this depends on the relative proportion of isotopes. The integer masses given in the task are actually the mass numbers of the most stable isotopes (they are all radioactive). See the [https://www.nist.gov/pml/periodic-table-elements NIST periodic table of the elements] for instance.

An interesting extension to the task could be to allow isotopes in the chemical formula.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 00:13, 20 March 2019 (UTC)

== precision of atomic masses ==
Since the atomic masses for some elements are expressed in greater precision,   shouldn't the computer programming solutions reflect that?   Most languages (at this time) aren't using enough arithmetic precision to give a precise result   (in particular, the formula that contains sodium).   Also, shouldn't the   '''assert'''   in the Rosetta Code task preamble be updated   (with greater precision)   for   '''sodium sulfate'''?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:09, 27 March 2019 (UTC)

:There is almost never a good reason to use the equality operator with floating point numbers. Especially in '''assert'''. Moreover, the mass of a compound is only as accurate as the least accurate component, so there is no point in dealing with arbitrary precision. Anyway, "usual" floating point numbers have nowadays 17 digits of precision. It's much more than the most accurate atomic mass. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 19:35, 1 April 2019 (UTC)

:: I wasn't referring to strangeness of how some languages represent floating point numbers   (those that aren't stored in decimal, but in some form of a binary format).   REXX (and maybe PL/I), for instance, use   ''decimal''   floating point numbers.   Also, because the REXX entry uses a more precise (accurate) table of atomic mass, the results would be correct, but not agree with the asserts.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:53, 1 September 2019 (UTC)
