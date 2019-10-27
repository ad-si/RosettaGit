+++
title = "Talk:Ramsey's theorem"
description = ""
date = 2014-04-20T11:26:59Z
aliases = []
[extra]
id = 11398
[taxonomies]
categories = []
tags = []
+++

==Solution(s)==
"... happens to have a special solution". Does that mean there are more solutions? [[User:Gaaijz|Gaaijz]] 14:09, 29 May 2012 (UTC)
== Task quality ==

Why is this task called “Ramsey”? Surely it's not just pining for a pretty little town on the Isle of Man? (A suitable link to Wikipedia in the task description would be good, and linking to somewhere like Mathworld ''as well'' would be better.) –[[User:Dkf|Donal Fellows]] 10:26, 14 February 2012 (UTC)
: http://en.wikipedia.org/wiki/Ramsey%27s_theorem [[User:Fwend|Fwend]] 19:59, 20 February 2013 (UTC)
:: This article actually mentions the term ''clique'': http://en.wikipedia.org/wiki/Tournament_%28graph_theory%29

== Revision of Task Description needed? ==

The task description seems to suggest that a solution is to generate graphs, test that the graph 
has the properties in Ramsays's theorem and then print any such graphs.

The solutions on the other hand cheat: they simply generate a known solution without 
checking that the graph has the sought after properties.

Either the task description should be changed to "Generate a graph" or it should
explicitly describe how solutions is to generate graphs and check for propeties.
<small>Written by [[User:Soegaard|Soegaard]] 12 June 2013‎</small>
:Agreed. All solutions are cheating. These programs are useless, except for showing how to build a graph, but the question is much more difficult. And basically, building a graph amounts to building two lists (vertices and edges). [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 16:54, 28 October 2013 (UTC)

:Suggest you add a proper algorithm in pseudo-code that implementers can follow as R.C. isn't usually about creating your own algorithm for a non-trivial task. For example [[Bulls and cows/Player]] and [[http://rosettacode.org/wiki/Quaternion_type]] describe at least one way of doing things. (But then [[24 game/Solve]] does not, although it did come with a Python solution that people seemed to understand). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:30, 28 October 2013 (UTC)

:MATHPROG gives notice of its intention to sue for libel those accusing it of cheating. It firmly asserts that it solves the task!! The task is to create a 17x17 matrix <math>G</math> of binary variable where <math>G[a,b] = G[b,a] = 1</math> means a is connected to b. Then create a list <math>N[a,b,c,d]</math> of all combinations of 4 nodes from <math>G</math>. For all <math>N</math>: <math>G[a,b] + G[a,c] + G[a,d] + G[b,c] + G[b,d] + G[c,d]</math> must be at least <math>1</math> (at least 1 pair connected) and no more than <math>5</math> (at least 1 pair not connected) as the values in <math>G</math> are changed. It may be better to require other values so that solutions can work up to 17x17. R(3,3) would only require 5x5 and R(3,4) would only require 8x8 for <math>G</math>.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:51, 30 October 2013 (UTC)

:: While I generally agree that the majority of “solutions” fail at the task as written, I'm not at all sure about how one could go about creating an ''algorithm'' to construct a graph from the requirements as listed. What would be possible would be to create the graph and check that the condition is true; a ''proof-checking'' operation, not a ''proof-generating'' operation. (I'm not quite sure what the lemma is that we'd be checking, probably that <math>R(?,?,?,?) > 17</math> as this looks like it's overlaying 4 connection patterns on the one graph, but that's of lesser concern.) Elevating to proof-generating would require graph generation, graph checking and sweeping up through the possible graph sizes, which would be potentially very expensive! Having a task that could require a significant amount of supercomputing time to execute without generating meaningful results earlier is a bit of a disqualification on the “good task” front, methinks. (There's multiple nested brute force steps in that outline; it would be nastily expensive to run.)
:: In any case, this task needs some significant revision work. It needs to be something which is practical, and yet not trivial. Merely generating the graph concerned is too trivial (we can have it as another task I suppose, but it's just too small a piece of working with Ramsey's theorem) yet doing proper proofs is too hard. I don't plan to attempt to solve this task (in Tcl, of course) until the task is worth solving. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:22, 22 December 2013 (UTC)

==Task extended==
OK, I've dealt with the thing about this (draft) task that was making me very unhappy. It now requires either that you ''search'' for a solution, or that you at least ''check'' that this special graph is a solution. (I suggest that most people will prefer checking; it's quite easy.) I'll go through and mark the existing task solutions as needing revision. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 11:26, 20 April 2014 (UTC)
