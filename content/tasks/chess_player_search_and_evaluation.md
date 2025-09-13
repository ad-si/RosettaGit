+++
title = "Chess player/Search and evaluation"
description = ""
date = 2015-05-18T18:57:42Z
aliases = []
[extra]
id = 19142
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task|Search and Evaluation}} This is a sub-task of the [[Chess player]] task. 

Create a function or software component which will search the tree of moves from a given chess position up to a maximum depth and determine the best move for the computer. Use a previously written [[Chess player/Move generation]] component for enumerating the moves. This is where a lot of effort has been spent to improve chess-playing programs, so you can create a basic version or implement some of the features mentioned below to get a faster or cleverer version.

Basic implementation:
#min-max search
#static evaluation based on material (numbers and types of pieces)

Advanced implementation:
#Alpha-beta, nega-scout or MTD-f 
#improved static evaluation using pawn structure and piece locations
#Iterative deepening and move ordering
#Quiescence searching extensions
#Null move heuristic
#Endgame database
#(very advanced) Optimizing board evaluation functions via genetic algorithms
