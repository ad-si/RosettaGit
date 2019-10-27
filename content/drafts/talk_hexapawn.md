+++
title = "Talk:Hexapawn"
description = ""
date = 2016-04-16T06:59:53Z
aliases = []
[extra]
id = 20798
[taxonomies]
categories = []
tags = []
+++

==Size of the task==

Judging by the size of the two programs (so far), this Rosetta Code (draft) task may be a bit much for most programmers to tackle (in whatever language they choose).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:08, 14 April 2016 (UTC)

The Pascal version is bloated by all the additional features: not just a NxM board size but various display areas showing the progress of the battle such as the changing state of the game tree and possible trace output, and attempting to select a good layout for a variety of possible screen sizes. Then there's the stuff for controlling the various options as play progresses (with an attempt to recognise mirror symmetry) and developing a summary of win/lose for each NxM option in special output files. Plus the storing of information in two working files via hash tables. A lot of this is overkill for a 3x3 board, for which the result is decided quite swiftly and with small amounts of memory required so that there's no need for hash tables and working files. The GO version omits a lot of this, but is still substantial. Actually, all these finite-state games are equivalent (provided that there is some resolution of draws and infinite loops, if they are possible) and one could imagine a game-playing system for which one would merely provide procedures for generating possible moves and suchlike specific to a given game, while the system provided the game tree analysis. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 06:59, 16 April 2016 (UTC)
