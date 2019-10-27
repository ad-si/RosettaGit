+++
title = "Talk:Greatest element of a list"
description = ""
date = 2018-10-08T11:59:33Z
aliases = []
[extra]
id = 10585
[taxonomies]
categories = []
tags = []
+++

== runtime values ==
"Create a function that returns the maximum value in a provided set of values, '''where the number of values isn't known until runtime'''."

Do ''any'' of these examples (other than the Lua one) meet this criteria? It seems to me that almost all of them are using a list defined ''before'' runtime. --[[User:Stuart P. Bentley|STUART]] 17:50, 27 September 2011 (UTC)
:Quickly skimming the solutions I'd say that the vast majority of the examples support lists where the values aren't known until runtime. It's just a lot easier to demonstrate using a hard-coded list. Pretty much any of the examples that have a function that takes a list (or array or whatever)--whether it's built-in or homemade--will be able to use a runtime list. They just don't because that takes lots of extra stuff like some other form of input besides hard-coding. --[[User:Mwn3d|Mwn3d]] 17:57, 27 September 2011 (UTC)
:I just changed the wording a little bit. I don't think it changes the intent of the task (and it's probably what they meant anyway). --[[User:Mwn3d|Mwn3d]] 18:02, 27 September 2011 (UTC)

:: I added an '''output''' section for the 3<sup>rd</sup> REXX example   (for the run-time requirement).   It does make this simple task a wee   (or just a wee-wee)   bit harder to read and understand the underlying program structure/logic;   for one thing, you now have to show the user's input(s)   (console/terminal/command line/parameters).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:20, 24 February 2017 (UTC)

==greatest element ...==
Does this mean the value, which may imply a numeric value?   Or can it also mean a lexicographic value?   If the values are numeric, shouldn't the largest numeric value be returned?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:06, 6 October 2018 (UTC)
:In OORexx the result of inheriting Comparable or Orderable and implementing CompareTo. Comparable in Ruby and Java. IComparable in .Net. Python 2 defining __eq__, __lt__ ...(Python 3 has a Comparable Class) etc.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:58, 8 October 2018 (UTC)
