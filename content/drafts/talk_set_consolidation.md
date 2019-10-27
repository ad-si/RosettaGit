+++
title = "Talk:Set consolidation"
description = ""
date = 2014-03-02T17:07:49Z
aliases = []
[extra]
id = 11691
[taxonomies]
categories = []
tags = []
+++

==Why.==
I needed this function to find all the ports on a net in an electronic [[wp:Netlist|netlist]] from huge lists of pairs of ports on all the nets of a design.
I tried to find out what the "computer science-type" name for the routine might be but failed. --[[User:Paddy3118|Paddy3118]] 10:59, 7 May 2012 (UTC)

:I'm not sure either, but if you break the sets down into sets of 2, it becomes the problem of finding the connected components of a graph, the sets of size 2 being the edges and the items being the nodes. --[[User:Spoon!|Spoon!]] 18:59, 7 May 2012 (UTC)

::Hi Spoon, thanks for the name '''"Connected components of a graph"'''. It lead to links such as [http://www.cs.umd.edu/class/sum2005/cmsc451/components.pdf this] and [http://computation.pa.msu.edu/NO/ConnCompPresentation.html this] that show that the set consolidation routine in Python could be applied to the type of problems described. --[[User:Paddy3118|Paddy3118]] 01:13, 8 May 2012 (UTC)

:There's no need to use permutation to show the result is order independent.  Think input sets as undirected graph nodes, and two nodes are connected if they share elements, then it's just a matter of finding connected subgraphs which clearly doesn't depend on input set ordering. --[[User:Ledrug|Ledrug]] 23:02, 7 May 2012 (UTC)

::Hi Ledrug, I wasn't sure of a general need either so left out mentioning order independence from the main task description but just wanted to make sure of my Python code so reported the run of it in the Python docstring. --[[User:Paddy3118|Paddy3118]] 01:13, 8 May 2012 (UTC)

::: Also task doesn't make clear what should happen when input contains empty sets. Consider this:
```python
def conso(s):
	if len(s) < 2: return s

	r, b = [s[0]], conso(s[1:])
	for x in b:
		if r[0].intersection(x): r[0].update(x)
		else: r.append(x)
	return r

print(conso([{1, 2}, set([]), set([])]))
```
 Is that what you want? --[[User:Ledrug|Ledrug]] 01:44, 8 May 2012 (UTC)

:::: Cool a recursive version. Maybe you could add it as a second Python implementation? Maybe I need to add that you should return your input when asked to consolidate less than two sets; as you have done.  --[[User:Paddy3118|Paddy3118]] 10:54, 8 May 2012 (UTC)

== Sets of Sets ==

This task specifies an intermediate result which can be a set or which can be a set of sets.  But this can become ambiguous if the input is a set of sets of sets.  I think it would be better if the task stuck with one level of abstraction for intermediate results (if arguments and all intermediate results were a set of sets). --[[User:Rdm|Rdm]] 22:35, 30 June 2012 (UTC)

:Hi Rdm, think of the intermediate as being consistently a set of set'''s''' where the number of set'''s''' can equal one; or more. --[[User:Paddy3118|Paddy3118]] 07:17, 1 July 2012 (UTC)

:: I did :)  --[[User:Rdm|Rdm]] 17:17, 1 July 2012 (UTC)

== Haskell ==
Is the Haskell version producing the right output for example 4?
--[[User:Soegaard|Soegaard]] ([[User talk:Soegaard|talk]]) 18:05, 17 May 2013 (UTC)

:Without any output, I cannot tell. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:14, 17 May 2013 (UTC)

:The output looks fine to me. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 17:07, 2 March 2014 (UTC)
