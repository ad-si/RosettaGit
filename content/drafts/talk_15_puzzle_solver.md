+++
title = "Talk:15 puzzle solver"
description = ""
date = 2019-02-05T17:25:55Z
aliases = []
[extra]
id = 21629
[taxonomies]
categories = []
tags = []
+++

== Mathematical meaning of minimum ==
The meaning of minimum has been discussed see: [http://www.rosettacode.org/wiki/Talk:Superpermutation_minimisation#Ambiguous Minimum]. It means 52 not 58, assuming fewest is a synonym for minimum. I think the task description should call for 'minimum solutions to random 15 puzzles' (see below)--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:28, 6 October 2017 (UTC)
:I am surprised that anyone thinks that 52 < 31. Optimal solution improved and now linked from the task description. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 05:11, 24 October 2017 (UTC)
:A fair point, but one that could have been made with less damage to the tasks structure I'm sure. I've clarified that only single moves are allowed in this task and created a draft task for multimoves, which are interesting in their own right but comparing them to single moves is silly--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:04, 24 October 2017 (UTC)
:Which may not be what you want. In the output you have "stm-optimal solution of 38(52) moves found in 1 minute and 54s: r3uldlu2ldrurd3lu2lur3dld2ruldlu2rd2lulur2uldr2d2" which is the second of the acceptable solutions in the task description before you changed it (only written not quite as specified in the task description). So with a little change of emphasis, and without marking other solutions as incorrect this could return.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:40, 24 October 2017 (UTC)
:Whatever. I can accept a moratorium on incorrect tags, splitting the task feels dishonest. Any clear, elegant solution, optimal or not, should be welcome. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 14:06, 25 October 2017 (UTC)
:Of course it's welcome. The stm-optimal solution is the objective of this task. It is not the objective of this task to find the mtm-optimal solution, but as an extra is not a problem.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:01, 25 October 2017 (UTC)

== Mathematical meaning of random ==
Unlike minimum, which I am surprised that anyone thinks means anything other than 'reduced to the least possible amount or degree', random means easy mathematically. There are 16!/2 15 puzzles, a little over 10 trillion, of which the number that are hard to solve is counted in the hundred thousands. Therefore a randomly chosen puzzle is easy. --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:36, 6 October 2017 (UTC)
:[http://www.rosettacode.org/wiki/15_puzzle_solver/20_Random see] for an analysis of 20 randomly generated 15 puzzles--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:00, 20 October 2017 (UTC)
::that page updated with a multimove solution [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 05:10, 24 October 2017 (UTC)

== Extra credit for non-random puzzles ==
We could offer extra credit for solving hard puzzles:

```txt

 2  1  3  4
 5  6  7  8
 9 10 11 12
13 14 15  0

```

and

```txt

  0 12  9 13
 15 11 10 14
  3  7  2  5
  4  8  6  1

```

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:08, 6 October 2017 (UTC)

== Multimoves ==
Prohibition on multimoves removed from task description. The task description should not dissuade such submissions. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 22:57, 29 November 2017 (UTC)

:I'd disagree... This means that some moves are more equal than others. Like, if one was considering distances, a (single-step) move is a constant unit of assessment, as with considering sort routines. A swap of elements ''i'' and ''j'' is three actions and does not consider the distance between them because the intervening positions are not involved. But, when moving a square multiple positions in the same direction, there are multiple actions, and, like Zeno's Arrow, each position is occupied... [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 00:29, 30 November 2017 (UTC)

::We have agreed that it is not fair or sensible to compare them. My point is about excluding valid solutions. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 02:52, 30 November 2017 (UTC)

::But comparisons seem possible and reasonable to me. Like, consider the "Optimal solution in 31 multimoves"
 u2r2d3ru2ld2ru3ld3l2u3r2d2l2dru3rd3l2u2r3dl3dru2r2d2            As offered.
 uurrdddruulddruuuldddlluuurrddlldruuurdddlluurrrdllldruurrdd    Run-length encoding undone: 60 moves.
 rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd            First solution, of 52 moves.
::This to me involves more (single-step) moves than the problem's two given 52 (single-step) move solutions and involves more movement of the squares, even though there are only 31 actions if you count the like of uu as one action. In other words, a minimum action-count problem is not the same as a minimum move-count problem since an action may involve multiple moves. Put another way, one could consider a set of allowable actions (here, udlr, or, udlr, uu ll rr dd, uuu lll rrr ''etc'') and once you start down that path, why not also allow additional actions, such as uld dru; whatever takes your fancy? Then start considering the minimum number of such actions required to transform one string into another... Perhaps not. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 08:39, 30 November 2017 (UTC)
:Hi Pete, Solutions which find multimoves also are not prohibited, the task requirement is to find the fewest single moves which the phix solution now does. It is on the page, it is not excluded, it is not required as part of the task. This problem can be solved by extension of  the A* algorithm. I didn't understand why you thought my suggestion to split the task was dishonest, it is my honest opinion that solving graphs with co-operative moves requires a different algorithm. That algorithm can also by used to solve this problem, apparently in 5.5hours rather than 29secs.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:20, 30 November 2017 (UTC)
==Using A* algorithm==
I added an extra credit to to [[A* search algorithm]] :

```txt

Use this algorithm to solve an 8 puzzle. Each node of the input graph will represent an arrangement of the tiles. The nodes will be connected by 4 edges representing swapping the blank tile up, down, left, or right. The cost of each edge is 1. The heuristic will be the sum of the manhatten distance of each numbered tile from its goal position. An 8 puzzle graph will have 9!/2 (181,440) nodes. The 15 puzzle has over 10 trillion nodes. This algorithm may solve simple 15 puzzles (but there are not many of those).

```

Using the better heuristic used in the Python A* solution enables the solution of the task problem (using 4GB of memory). [[15 puzzle solver/20 Random]] has 20 random starting positions. Using the Python code starting positions requiring 1sec or more can not be solved on a computer with 8GB of memory. With 8GB it can test up to say 2200000 positions. So these solutions are good for extra credit medal  with bar in the A* task but do not indicate that this is the correct algorithm to solve this task--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:25, 5 February 2019 (UTC)
