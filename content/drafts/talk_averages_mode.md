+++
title = "Talk:Averages/Mode"
description = ""
date = 2010-02-06T12:29:16Z
aliases = []
[extra]
id = 4380
[taxonomies]
categories = []
tags = []
+++

==Typing==
Rereading the task text to check my C implementation compliance, I've noticed it asks the mode for a vector of integers. Shouldn't this requirement be relaxed, e.g. with "vector of numbers" instead? --[[User:ShinTakezou|ShinTakezou]] 13:33, 14 June 2009 (UTC)
:I think it can be open to all types of objects. Since all we need is a count of how many of each thing there is, and don't need to do arithmetic on the values, they don't need to be numbers, right? Perhaps we can allow them to use a collection of integers or floats or other objects, whichever is easiest in their language. --[[User:Spoon!|Spoon!]] 03:08, 15 June 2009 (UTC)
::The Tcl version can also do any type too as it happens (strictly any value supporting comparison for equality with another value, but all values do in Tcl). I avoided saying "numbers" because it's much less clear as to what that means and I want to keep things simple for the strictly-typed folks. —[[User:Dkf|Donal Fellows]] 08:10, 15 June 2009 (UTC)
::Also, I used “vector” because that language was used in several related tasks. —[[User:Dkf|Donal Fellows]] 13:57, 15 June 2009 (UTC)

==Similarity with other tasks==
This task is similar to [[Anagrams]]. Perhaps we can borrow ideas from that page. --[[User:Spoon!|Spoon!]] 03:08, 15 June 2009 (UTC)
: There are also similarities with [[Image histogram]] really. All are constructing histograms over some set of values. —[[User:Dkf|Donal Fellows]] 08:10, 15 June 2009 (UTC)
: Also [[Max Licenses In Use]] --[[Special:Contributions/76.173.203.58|76.173.203.58]] 18:49, 18 June 2009 (UTC)

== The name of the task ==

The name of the task, "Mode", is not very descriptive when seen out of context (e.g. in the alphabetic list of tasks).
Maybe it should be renamed to something like "Mode (statistics)"?
--[[User:PauliKL|PauliKL]] 14:58, 14 August 2009 (UTC)
:That sounds OK to me. --[[User:Mwn3d|Mwn3d]] 15:05, 14 August 2009 (UTC)

How about subpage style: [[Average/Mode]]? Then we can have [[Average/Median]], [[Average/Harmonic mean]] and so on [[wp:Average#Types|and so forth]], and all of them will sort together in one place in task lists. --[[User:Kevin Reid|Kevin Reid]] 16:05, 14 August 2009 (UTC)
:Let me expand on this: Does anyone ''object'' to my renaming [[Mean]], [[Median]], [[Mode]], [[Standard Deviation]], [[Moving Average]] according to this scheme? (I would also rename [[Standard Deviation]] to [[Average/Running standard deviation]] so that there's an obvious place for an all-at-once standard deviation.
:: No objection to the scheme, but might I suggest "statistics" instead?  Or the more traditionally wiki-esque "Mean (statistics)", "Median (statistics)", etc? --[[User:Short Circuit|Short Circuit]] 03:14, 15 August 2009 (UTC)
:::Statistics is a very broad category covering a ''lot'' of different kinds of tasks, whereas averages all have something in common. WP says:
::::An average is a single value that is meant to typify a list of values. If all the numbers in the list are the same, then this number should be used. If the numbers are not all the same, . . . the average is calculated by combining the values from the set in a specific way and computing a single number as being the average of the set.
:::This seems to me to be a useful classification. (I now realize my inclusion of Standard Deviation in the list above was incorrect; while it does compute a single number from a collection, that number is not an average.) Using a prefix rather than a suffix has benefits for sorting. Additionally, averages are not merely statistical techniques: they can give exact answers to certain problems. (Er, I think. I'm having trouble thinking up an example of this at the moment (and it's rather late).) --[[User:Kevin Reid|Kevin Reid]] 03:56, 15 August 2009 (UTC)
::::If you pull standard deviations from the list, then "average" is fine.  I won't press the point on disambiguation syntax. If you're looking for problems where averages can give exact or non-statistical answers, you might look at physics, motion and prediction. --[[User:Short Circuit|Short Circuit]] 08:03, 15 August 2009 (UTC)
:::::Moving average is normally used as filter rather than as a statistical function. But if the page Average is created, I guess Moving Average should be there, too. BTW, Is there an example of sub-pages in RC somewhere? --[[User:PauliKL|PauliKL]] 11:59, 17 August 2009 (UTC)
::::::Yes, the Loop/ tasks, [[Loop/Break]], [[Loop/Nested]] etc. --[[User:Kevin Reid|Kevin Reid]] 12:41, 17 August 2009 (UTC)
