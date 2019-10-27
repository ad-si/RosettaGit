+++
title = "Talk:Red black trees"
description = ""
date = 2016-07-15T22:07:36Z
aliases = []
[extra]
id = 21002
[taxonomies]
categories = []
tags = []
+++

Each node has only 3 states instead of 4 states as with AVL Trees. This may have implications in biology. It would be interesting to know whether the search trees of the brain are 2 state, 3 state or 4 state, Red/Black or AVL. Two states may be more susceptible to representation in biology. As an aside, 4 state trees can be made into 3 state trees if each node has a separate isHeader boolean. Using the same stategy, Red/Black trees can be reduced to 2 state trees - which biology could easily handle. So I'm guessing that our brains have Red/Black trees in them. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 10:55, 11 July 2016 (UTC)

Someone on the internet, I forget who, says that red/black should be done from the top down rather than bottom up. The algorithms presented here are bottom-up. In fact, they are a port of Borland's C++ STL implementation. Note that they are not as simple as the AVL implementation in C#. RotateLeft and RotateRight have two parameters instead on one (as for AVL). [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 11:29, 11 July 2016 (UTC)

== Is this supposed to be a task? ==

(I thought we already had a red-black tree task, but I can't find it - I don't know if that's because of bit rot or because we never had one...)

If this is a [[Rosetta_Code:Add_a_Task|task], it should have a task description, at the very least. If this is meant to be part of some other task, it should link to that (if it still exists). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:18, 15 July 2016 (UTC)

:: The red-black tree page is under [[Pattern_matching]]. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 22:06, 15 July 2016 (UTC)
