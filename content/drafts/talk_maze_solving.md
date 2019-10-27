+++
title = "Talk:Maze solving"
description = ""
date = 2010-12-23T08:08:31Z
aliases = []
[extra]
id = 8985
[taxonomies]
categories = []
tags = []
+++

== PicoLisp Example ==

The output of the PicoLisp example (the initial implementation in this draft task) doesn't appear to solve the maze.  The last mark is in the SW (bottom left) corner, whereas the exit appears to be in the SE (bottom right) corner.
--[[User:DanBron|DanBron]] 17:11, 15 December 2010 (UTC)
:"Solving" means here to find the shortest path to the next exit. The example passes 'a1' as the first argument, and plots the path to the next exit ('a12' in this case). If we gave 'a12' as the starting point, the search would be over immediately, as the exit is just there. --[[User:Abu|Abu]] 17:32, 15 December 2010 (UTC)
::I tried to make the text more clear. Hope it is better now. --[[User:Abu|Abu]] 17:36, 15 December 2010 (UTC)
:::I just tried to clear it up a bit more. We assumed that a1 was the entrance, p12 was the exit, and this program would find the path from the entrance to the exit. It turns out it assumes that a1 and p12 are BOTH exits and the player starts somewhere in the middle of the maze. --[[User:Mwn3d|Mwn3d]] 17:43, 15 December 2010 (UTC)
::::Probably the way I defined it is confusing. I assumed that when you are in a maze you want to find the shortest way *out*, but when looking at the diagram I understand that this might not be what people expect. Shall we change the spec so that the shortest path between the two exit points is plotted? --[[User:Abu|Abu]] 18:10, 15 December 2010 (UTC)
:::Changed it. Seems indeed to be much clearer now :) --[[User:Abu|Abu]] 18:27, 15 December 2010 (UTC)

Hi Dkf, displaying the direction with arrows is a good idea :) --[[User:Abu|Abu]] 19:12, 22 December 2010 (UTC)
: Glad you liked it. I put it in to better show off that the path was understood. –[[User:Dkf|Donal Fellows]] 08:08, 23 December 2010 (UTC)
