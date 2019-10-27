+++
title = "Talk:Decision tables"
description = ""
date = 2011-07-14T20:17:28Z
aliases = []
[extra]
id = 9186
[taxonomies]
categories = []
tags = []
+++

How much of the decision table should we model? In particular, the “condition alternatives” quadrant has been omitted from both current examples, as both are using “complete” alternative tables in their model, but surely it would be better to have that data anyway? If nothing else, it would possibly permit a more compact representation when using larger numbers of questions... –[[User:Dkf|Donal Fellows]] 12:01, 25 January 2011 (UTC)

:Good point. Protium does the condition alternatives part -- I'll get to an example real soon now. ---[[User:Axtens|Axtens]]

== Visual layout ==

From the WP article and its linked references, it seems that certain aspects of the row/column layout of the table are important.  It seems important that action indicators (X) are kept visually alligned with their associated condition values (Y and N,) that the condition text is kept visually associated with the condition values, and that the action text is kept visually associated with the action indicators.  It seems a key feature of decision tables is that their row/column layout makes them easy to understand and easy to maintain.  Some solutions posted do better at this than others.  Maybe this property of decision tables should be stressed or required in the task description?

Also it should be clear how the decision table is to be demonstrated and what is expected for output.  Should there be a whole user interface for a support script?  Should it return actions after each question, or only after all questions have been answered?  Should it generate an optimal script to minimize the number of questions required given the table and past history of support calls?)

Or, without doing the support thing, is it enough to just show the actions for an example set of conditions?
