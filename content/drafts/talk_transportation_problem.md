+++
title = "Talk:Transportation problem"
description = ""
date = 2016-07-20T12:44:06Z
aliases = []
[extra]
id = 16031
[taxonomies]
categories = []
tags = []
+++

It is pretty interesting the task. Why no one wants to try to make solutions in the own language? [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 20:02, 24 August 2013 (UTC)

:Hi Русский, for me, the problem is that I cannot easily see an algorithm to solve the problem and cannot read the non-english example to produce an example in Python for example. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:56, 25 August 2013 (UTC)
:: But it's a classic of linear programming, which is studied in any technical college. [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 11:11, 27 August 2013 (UTC)
::: Then the algorithm should be easy to describe. But I cannot find a description of the algorithm by searching for "method of potentials (with redistributive cycle)" so that is not a useful description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:40, 4 August 2015 (UTC)

:I've been too busy with (many!) other things for much Rosetta Code problem solving. I've only been doing things the past few days because I've had some time off work (long weekend/public holiday/:-) :-) :-)). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 21:51, 25 August 2013 (UTC)

* So, added the implementation in the language with English lexical base. Now the algorithm will be more clear. It is based on the construction of the redistribution cycle. It is a closed line connecting certain way the occupied cells of the distribution plan. After building the cycle and redistributing the supplies, the procedure is repeated until you reach the optimal supply plan. [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 08:38, 25 January 2014 (UTC)

* Maybe should add a more detailed description of the algorithm of solving this problem? [[User:Русский|Русский]] ([[User talk:Русский|talk]]) 09:45, 9 February 2014 (UTC)

: That would be nice - right now, the implementations here seem to be messy and overly complicated. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:54, 4 August 2015 (UTC)

:: Have you read the page at the first link I provided? It's not very long. You'll see it's not as simple as you think. After an initial estimate, you need to find the optimal solution, that's where the catch is. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 19:12, 13 July 2016 (UTC)

::: Why is this even a question? 

::: If you can't put the algorithm description on the task page, and if you can't include task examples which illustrate whatever issues concern you, then maybe the task doesn't belong here on this site. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:27, 13 July 2016 (UTC)

:::: Yeah, the task description is not well-phrased, I've been thinking about improving it (I didn't write it) but I looked into this task almost 2 years ago, so it's no longer clear in my mind. For the time being, the link I provided is your best bet. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 19:33, 13 July 2016 (UTC)

::::: After re-reading http://orms.pef.czu.cz/text/transProblem.html which is what is currently linked in the task description. I am not convinced I need to make any changes to my stupid implementation. The northwest corner method is about coming up with an initial feasible solution, so that other methods may be used, and I'm using that. Presumably the "method of potentials (with redistributive cycle)" would involve additional work, but for the task example there is no additional work to be performed. Also, there's no description of this method at that link.  (And, yeah, I sort of assumed I had already conveyed this idea, but since the task author(s) presumably sort of assumed that they had adequately described the task, I figured that if I can expect more of them I should also expect more of myself.)

::::: So, just to re-iterate: task needs a better example. Task needs a better description.

::::: And, for example, costs of 4 6 8,: 4 2 10 would probably be better than what's currently listed. But an example with more than two suppliers, more than two consumers would be a much better test of this kind of code. If possible, also, we should have a local optimum cost near the values supplied by the northwest method which is not the global optimum.

::::: Anyways... for now, the task itself is defective and I think we need at least one implementation which clearly illustrates this defect. And, since no one else has volunteered to be the village idiot, I am electing myself to that position. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:43, 15 July 2016 (UTC)

::::: That said, the [[Brace expansion]] task might be a good example of for how a decent task specification should look? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:44, 20 July 2016 (UTC)

==Task Description==
I suggest this task is shortened by assuming the starting point is a good initial basis for a feasible solution. Code for the NW allocation would then not be required given that other methods for generating an initial basis are available--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:52, 18 September 2014 (UTC)
: I fail to see the benefit. The NW method is too simple to be given its own task page, so it's nice to have it included in this one. More sophisticated methods can be handled in separate tasks and we can link to them from here. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 12:47, 18 September 2014 (UTC)
:: Also I think it's better for testing purposes if the initial feasible solution is off the mark. That way your optimal solution routine is more pressured to produce a correct result than when you have an estimate that may already be correct to begin with. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 13:16, 18 September 2014 (UTC)

==Regarding The Java/D Versions==
I have converted the Java version to D language. I've used a class Shipment as in  the Java code. Perhaps there are good enough ways to use values (a matrix of structs) instead, and reduce indirections.

I have found the function getClosedPath() not easy to understand and semantically a little complex (and bug prone to translate). So can you comment the parts of getClosedPath() to help me write simpler D code, or can you restructure it to make its workings more linear (in D I've used three different 'stones' variables inside it, including a dup-ping)?
: Yes, I was planning to add a comment there, it's not very clear. I think you're supposed to use a breadth-first or depth-first search, but it occurred to me that if you remove all the "stones" that do not have a horizontal AND vertical neighbor (and therefore cannot be corner-stones) you will be left with the stones that form the closed path. (Removing stones is done in a loop because certain stones will only be removed after you've removed another.) Then you have to put the remaining stones in the correct plus-minus order. (See also the link to the stepping stone explanation). [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 21:28, 6 November 2014 (UTC)
