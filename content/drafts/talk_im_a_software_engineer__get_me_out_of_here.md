+++
title = "Talk:I'm a software engineer, get me out of here"
description = ""
date = 2019-09-20T04:39:23Z
aliases = []
[extra]
id = 21959
[taxonomies]
categories = []
tags = []
+++

==Algorithm==
I think the use of Dijkstra's and Floyd-Warshall algorithms should be a suggestion rather than a requirement.
Anyway, I've used a simple breadth-first algorithm for the Phix entry. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 21:00, 17 August 2018 (UTC)
:Thank you for your contribution, sadly your last as I read in this morning's standing orders that you were shot at dawn. Part 1 is to find a list of the shortest routes to safety. I interpreted this as meaning all points of safety which could be reached in 5 days. Having supplied only one which turned out to have LARC (Liberation Army of RosettaCode - objective Liberate the president's gold) waiting, the president assumed you were part of the conspiracy. Just because he's paranoid it doesn't mean no-one is out to get him. Further the extra credit is "Which cells will it take longest to send reinforcements to from HQ?". HQ is at (11,11) (or 12,12 if indexing starts at 1 as God intended), so I'm not sure what {"show_longest",{{23,13},{21,15},{4,20},{7,21},{18,21}}} answers. At the end of 2011 people were asking why RC's Dijkstra task was still draft. Mid 2018 I have updated that task so that it may be implemented consistently and raised it to full status. One of the requests was for a non trivial example, I would like this maze to be that. They also suggest that Floyd could be used equivalently, which may be for the trivial examples they give, tried here one sees the difference. So I think this task's specification is clear and should be adhered to.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:04, 29 August 2018 (UTC)
::Reports of my demise have been greatly exaggerated, the firing squad were all cross-eyed and missed. Perhaps some sort of proof that using those specific algorithms are better in this case than some other alternative should be shown (in the output, dunno what though).
:::Explains a lot about our current situation. Perhaps the president needs opticians, not software engineers. Probably wouldn't make a difference, for a few dollars they'd be blind. Who can blame them given the president's example?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:20, 8 September 2018 (UTC)
::The edges are not weighted (they all cost 1 day), so Dijkstra is an unnecessary overhead compared to breadth-first, albeit the relatively small one of maintaining and retrieving the lowest-cost node next (in my code the "next" variable only ever holds {cost}{cost+1}). Perhaps if the numbers on the map, instead of some magical "teleport" number were a terrain difficulty, with 1 being "straight fast motorways" and 9 being "dense undergrowth, steep difficult climbs, boggy marshes, minefields, and similar obstacles", so to move 1 square costs (this+dest) days travel, then that might justify using Dijkstra. It would of course mean there are no unreachable cells.
:::Sounds good, two examples sometimes better than one. Do you have one in mind?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:58, 19 September 2018 (UTC)
::As it stands, Part 2 can be completed with a simple breadth-first search (or two), calculating at most 820 routes, whereas using Floyd-Warshall creates 170,156 routes. When you say "tried here one sees the difference", did you mean a large negative one?
:::At the briefing it was "concluded that you need to know the shortest route from each cell to every other cell". The commanders are not going to tell you where their troops are or where they want them. The two routes are just examples, a database with all routes is required.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:58, 19 September 2018 (UTC)
::You could justify Floyd by asking for the maximum shortest route between any two points, so I've added that to the task and done just that.
:::Good for you. An improvement.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:20, 8 September 2018 (UTC)
::Lastly, you didn't understand the show_longest output, so, as well as showing all 40 shortest routes, I have extended the output of show_longest to show the full routes for those 5 nodes, maybe that will make more sense to you. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 13:34, 30 August 2018 (UTC)

==40 or 71 routes==
The Julia example shows 71 routes. It has 4 that end at (3,3), whereas the others have only 1 (and Phix has only 1 that ends at {4,4}).
As above I thought Dijkstra's ought to be a suggestion anyway, and apart from the fact that should only give 1, I have no preference 
for whether both are acceptable, but if so the task should probably mention that. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 00:54, 20 September 2019 (UTC)

The Julia LightGraphs module actually only gave 1 route from its Dijksrta implementation. Based on the extant examples I thought that must be wrong, but the simplest solution was then to get all routes.
--[[User:Wherrera|Wherrera]] ([[User talk:Wherrera|talk]]) 04:39, 20 September 2019 (UTC)
