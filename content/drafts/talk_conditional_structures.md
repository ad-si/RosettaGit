+++
title = "Talk:Conditional structures"
description = ""
date = 2010-02-06T12:50:00Z
aliases = []
[extra]
id = 1790
[taxonomies]
categories = []
tags = []
+++

Hey, about the "See Also" thingy, at the bottom of the screen, I first added them to complement about the PHP stuff, but now that got me thinking... Would it be interesting to add links to relevant docs, for this matter at least, and for every language we know of ? Instead of just PHP? Also, if we do this, what would be more appropriate: adding an "See Also" to every language, or one, at the bottom that regroups all the info ? --[[User:CrashandDie|CrashandDie]] 12:14, 25 January 2007 (EST)

== Breakout? ==
Would it be appropriate to break this task out like we did with [[Loop Structures]]/[[Iteration]]? --[[User:Mwn3d|Mwn3d]] 16:39, 21 October 2008 (UTC)
Seriously. This page is gigantic. --[[User:Mwn3d|Mwn3d]] 20:32, 15 September 2009 (UTC)
: The real problem is that there are masses of languages that are participating in this. Most provide just a short answer too; sub-pages are likely to be long. I suppose this is success... —[[User:Dkf|Donal Fellows]] 22:29, 15 September 2009 (UTC)
:: It may be practicable to use conditional transclusion.  Specifically, if a page body exceeds a certain size, don't link to it.  If it doesn't exceed that size, transclude it.  For "print" view, transclude all of it.  That cleanly handles everything from User Output to RCSNUSP. Doing the conditionals in wikicode would depend on having a way to measure the size of another page, though. --[[User:Short Circuit|Michael Mol]] 22:59, 25 November 2009 (UTC)

== syntax highlighting ==

Seem to have missed a page for syntax highlighting conversion.  I don't have time to fix it atm. --[[User:Short Circuit|Short Circuit]] 01:51, 1 April 2009 (UTC)

== What is a Conditional Structure? ==

What is a conditional structure?  Is a try/catch statement a conditional structure?
--[[User:Rdm|Rdm]] 21:31, 6 October 2009 (UTC)
: See [[wp:Conditional (programming)]]. Basically, a choice has to be made, and code is or isn't executed based on that choice. (In many languages, that choice usually boils down to "true" or "false".) Try-catch falls under [[Exceptions|exception handling]]. (Technically, I suppose that it could be considered a conditional, where the condition is "works" or "error".) -- [[User:Eriksiers|Eriksiers]] 22:09, 6 October 2009 (UTC)

: I would support Eriksiers meaning too. It is a boolean choice between one or many paths. (It would also fit most, if not all the examples given). --[[User:Paddy3118|Paddy3118]] 03:10, 7 October 2009 (UTC)

:: I have added [[Conditional Structures#conditions without conditions|conditions without conditions]] to the J writeup.

:: In J, data selection (which I have illustrated by example) has the same sort of prominence that short circuit condtions have in some other languages.  In both cases, we have conditional logic which fits inside expressions.  And in both cases we have some potential efficiency gains because we might be avoiding unnecessary computation.  But data selection tends to be far more efficient in J than short circuit conditions (and low level mechanisms such as [[wp:Branch predictor|branch predictors]] suggest that this issue might not always be unique to J). [[User:Rdm|Rdm]] 20:57, 25 November 2009 (UTC)
