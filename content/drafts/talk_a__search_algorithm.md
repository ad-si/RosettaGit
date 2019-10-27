+++
title = "Talk:A* search algorithm"
description = ""
date = 2019-09-24T01:01:49Z
aliases = []
[extra]
id = 21292
[taxonomies]
categories = []
tags = []
+++

== moving into a barrier position ==
How does a path   ''move into''   a barrier position?

I see that none of the programming examples show a path (so far) that "moves into" a barrier,  

else we'd be seeing a total cost for a path that exceeds   '''100'''.

Does there need to be a cost when "moving into" a barrier, since no path has (apparently) done that?

How would one show a path that   ''moves into''   a barrier?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:42, 29 January 2017 (UTC)

:While A* does not evaluate every possible move, it does internally check the cost of moving into a barrier square. For this reason, the cost of moving into a barrier square is required. However, there is always a lower cost alternative while still moving in the correct general direction (according to the heuristic), it should never actually be part of the maze solution.

:As you pointed out, the method for showing a path that moves into a barrier is left undefined. However, it should not be part of the a* solution so any method would be fine.--[[User:TimSC|TimSC]] ([[User talk:TimSC|talk]]) 16:58, 29 January 2017 (UTC)

:: So if a "move into" a barrier is never shown (because there is always a lower cost solution), why have a cost (for moving into a barrier) at all?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:48, 29 January 2017 (UTC)

::: In terms of understanding, there are two approaches where the "move into barrier" may lead:
::::# The barrier is a '''soft''' restriction. This means one '''may''' consider the barrier to find the path, and it will be considered when there are no paths without barrier move. In terms of coding, this places the barrier move in the queue with the least priority by assigning a huge cost. Then, at the first time a path successfully is found, the high cost will indicate a barrier was crossed.
::::# The barrier is a '''hard''' restriction. This means when there are no valid paths, the algorithm will return an '''empty path'''. In terms of coding, when the move puts a barrier, the move '''will not be added to the queue'''. Then, as the priority queue is popped, if there aren't any valid paths, the queue will empty and the A* shall raise the problem. 


-----



:: Also (above), you mentioned that:   ''A* doesn't evaluate every possible move''. 

However, in the Wikipedia article (link), it states: 

''A* is an informed search algorithm, or a best-first search, meaning that <u>it solves problems by searching among all possible paths to the solution (goal)</u> for the one that incurs the smallest cost (least distance travelled'' (sic), ''shortest time, etc.), and among these paths it first considers the ones that appear to lead most quickly to the solution.''   ...     (underscoring and italics added by me).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:59, 29 January 2017 (UTC)
::: A* does not ''need'' to evaluate every possible move if two things happen:
::::# h(x) is always lower than the actual remaining cost.
::::# h(x_1) &lt; h(x_2) + d(x_1, x_2).
:::

== grid orientation ==
It would seem that some programming examples are using a non-standard orientation of the

grid display,   with the   '''(0,0)'''   origin point in the   top-left   of the display area,   with positive

values for   '''X'''   (columns)   ''going downward'',   instead of   ''going upward''. 

For me, it doesn't make me no never-mind no-how, but it took a wee bit of fixin' for my 

programming example   (not yet posted)   to match the existing displayed grids.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:42, 29 January 2017 (UTC)

==A more interesting example==


A barrier is created by setting a square's value effectively to infinity. How this is achieved by the algorithm should be implementation dependant. Some languages support the concept intrinsically, certainly 100 should not be a magic number. Would it not be more interesting if the squares had values other than 1 or infinity. Say randomly assigned?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:33, 30 January 2017 (UTC)

==Related Tasks==


How is this algorithm related to solving a Hidato Puzzle? or the others for that matter. Does anyone intend to produce a solution using this algorithm?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:33, 30 January 2017 (UTC)
 
: The <u>algorithm</u> used for the '''A*''' search isn't related to a Hidato puzzle, but parts of the (computer program) solution may be used, especially the presentation of the solution/answer(s), but that's not the reason I added those other tasks; they appear to me as related tasks in that quite a bit (at least my solution, as yet not included) had a lot of code in common.   --- Not that everyone else may have had the same observation.   I never intended to imply that the same algorithm (or a even a modified one) could/would/should be used (or even considered) for those other related tasks.   In no way does a related task imply the same algorithm could or should be used or applied to another task.   To answer your second question, no, I am not going to use this algorithm to solve   ''any''   other Rosetta Code problem.   That's not what a   ''related task''   means to me.   I also believe that   ''related''   doesn't necessarily mean   ''similar''   in this context.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:41, 30 January 2017 (UTC)

:Wherein lies the problem. Do you think that the Rexx (and some other) solutions implement the '''A*''' search algorithm? From Wikipedia:

::Typical implementations of A* use a priority queue to perform the repeated selection of minimum (estimated) cost nodes to expand. This priority queue is known as the open set or fringe. At each step of the algorithm, the node with the lowest f(x) value is removed from the queue, the f and g values of its neighbors are updated accordingly, and these neighbors are added to the queue. The algorithm continues until a goal node has a lower f value than any node in the queue (or until the queue is empty).

:Specifically, there is no back-tracking in '''A*'''. Of course the task description does not clearly specify what is meant by '''A*''' but if anything goes then I think '''A*''' should be removed from the task's title allowing any way to find this path. Alternatively the task description should be clear and some of the solutions marked incorrect.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:39, 21 November 2017 (UTC)

== Extra credit ==
While it is perfectly possible to solve an 8-puzzle with A*, and it is commonly used/taught, the fact that it is completely impractical for a 15-puzzle gives me serious doubts. Perhaps something more like the javascript demo, showing nodes actually examined would be better.
