+++
title = "Talk:Simulated annealing"
description = ""
date = 2019-10-07T13:49:33Z
aliases = []
[extra]
id = 20665
[taxonomies]
categories = []
tags = []
+++

==Task should be more specific==

This looks like it could be a fun task. But currently it asks for 100 cities, without specifying the travel costs between these cities. (It also leaves out the definitions of the names in the pseudocode - something which currently requires a visit to the wikipedia page.)

We're going to need a specific task before we can have valid implementations. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:23, 23 March 2016 (UTC)

The travel cost between two cities is the distance between these cities (added in the task description).
The definitions should be in the notations paragraph. Could you tell what is missing ? Thx.
--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 17:13, 23 March 2016 (UTC)

: Those costs are missing. There are up to 10000 costs to be considered for this case - they could be specified by posting them to a page or algorithmically, but currently I do not know what those costs should be. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:23, 23 March 2016 (UTC)

:: The cost is the euclidian distance, and the distance is the cost. 4 exemples are given. To compute the cost between two cities a and b at (xa,ya) (xb,yb), use sqrt (xa-xb)^2 + (ya - yb)^2 .--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 17:28, 23 March 2016 (UTC)

::: Oh, I see it now. I'll try that. Thanks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:42, 23 March 2016 (UTC)

:::: Added precisons about the cities location- Were needed - Thx - --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 18:03, 23 March 2016 (UTC)

::::: Another issue is this: ''Pick a random neighbour city v > 0 of u , among u's 8 (max) neighbours on the grid.'' vs ''The cities are '''all''' connected.'' I'm having a problem figuring out how this pair of constraints makes sense. (More specifically - what does this pair of constraints mean for my data structures?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:45, 23 March 2016 (UTC)

:::::: Means that the graph is complete : you can go in one step from any city to any other one, using a single edge. This simplifies things, there is no need to describe the cities graph. A path (permutation) is always legal. The neighbours of a city  u are the closest, at distance 1 or diagonally at distance √2. For example the neighbours of 0 are 1, 10, 11 . The neighbours of 37 are 36,38,47,27 and 46,28,49,26. When you pick 37 (at random) you choose (at random) one of oits eight neighbours.  We swap only neighbours in order to have small perturbations at each step.--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 22:14, 23 March 2016 (UTC)

== k and kT ==

Can the role of k in the annealing analogy be addressed in the Notations section?  Also kT. In other words, 
what do k and kT represent in a metallurgical annealing process?

Thanks,         ... Peter E.

: I am pretty sure you mean the 'k' which is described in the task description at the top of the page, and not the language k. But, for a start, please take a look at the top of the page and see if that addresses your concerns... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:33, 29 September 2019 (UTC)

: After thinking about this further (and trying to address Peter E's concerns), it looks like this issue resides in the original draft of the linked wikipedia page (and persists in the current pseudocode).

: Quoting the current wikipedia page: "The annealing schedule is defined by the call temperature(r), which should yield the temperature to use, given the fraction r of the time budget that has been expended so far." This might suggest that k in the ratio k/kMax starts at kMax and falls to 0, but the pseudocode explicitly states the opposite. 

: For compatibility with the pseudocode, we've used the expression (1-k/kMax) for r, instead of a simple ratio and made the function temperature(r) be kT*r where kT is 1...

: Of course, we could have used any of a variety of other expressions for the temperature function, and the cooling aspect does not specifically have to be encoded into r. So -- with an overly narrow focus -- it's easy to argue that the existing approach is "just fine".

: But it's also fair to be confused by the current approach, and to ask questions about it... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:03, 30 September 2019 (UTC)
