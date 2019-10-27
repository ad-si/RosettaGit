+++
title = "Talk:Permutations by swapping"
description = ""
date = 2012-11-04T07:49:18Z
aliases = []
[extra]
id = 12111
[taxonomies]
categories = []
tags = []
+++

== Does "Python: recursive" fit this task? ==

The "Python: recursive" solution doesn't seem to use swaps, which I thought was the whole point of this task. We already have a [[Permutations]] task for generating permutations in general. --[[User:Spoon!|Spoon!]] 17:58, 26 July 2012 (UTC)

: It also doesn't generate the output requested (it's missing the signs). Not that I think it should, though; right now, too much of this task smells of “let's write the task to force my code to be a solution”. OTOH, if I was to take the version that ''does'' formally satisfy the task as described, it wouldn't be usable in the other task linked. Oh dear, oh dear! I think this draft task needs cleaning up, and the cleaning needs to be done by someone other than the original author with an eye to maximizing coherence and the ability to implement reasonably in many languages. –[[User:Dkf|Donal Fellows]] 19:27, 26 July 2012 (UTC)


Hi Spoon, Donal; A quote from the Wikipedia article:
: ''"Each permutation in the sequence that it generates differs from the previous permutation by swapping two adjacent elements of the sequence."''
Do I need to make clear that the program does not have to generate using swaps - only that the above holds true? --[[User:Paddy3118|Paddy3118]] 20:11, 26 July 2012 (UTC)

:Yep the task description did need tidying up, which I have started.
:The task was written so that it could be a precursor to a method for calculating a [[Matrix arithmetic|determinant]] which needs a sequence of permutations and matching sign. (The sign is generated in the recursive solution Donal, and the outputs of both Python solutions match).
:Hopefully with these modifications the task should be in a fit state to be attempted in other languages. --[[User:Paddy3118|Paddy3118]] 20:31, 26 July 2012 (UTC)

::So the order of the output doesn't matter, as long as it is true that adjacent results differ by a swap of adjacent elements? How would one verify this easily? Perhaps a function to verify that the results satisfy this should be a part of the task, because it is insufficient to simply show that all the permutations are generated. --[[User:Spoon!|Spoon!]] 00:07, 3 August 2012 (UTC)
:::Well:
:::* If the items (of each successive perm), doesn't change. (Sort them and compare).
:::* And if exactly two items are not in the same position as before.
:::* Then the two consecutive permutations are OK.
:::--[[User:Paddy3118|Paddy3118]] 07:35, 3 August 2012 (UTC)
:::: This is confusing.  If the task is to use the Steinhaus–Johnson–Trotter algorithm then the permutations must appear in a specific order.  If the task is to find the parity of a permutation, then we do not need the Steinhaus–Johnson–Trotter algorithm.  So... what's the task?  --[[User:Rdm|Rdm]] 07:58, 3 August 2012 (UTC)
:::::That particular ''algorithm'' is given as an example, but you are free to use another that fits the requirements. In the explanations I read of the SJT algorithm, they mentioned swapping quite a lot - hence the task title.

== bear in mind? ==

Currently the task says "Such data are of use in generating the determinant of a square matrix and any functions created should bear this in mind."

But what does this mean?  Functions do not have minds...  (Or, being slightly less flip: this feels like it could easily become a modularity violation if it were refined into something testable.)  --[[User:Rdm|Rdm]] 13:22, 30 July 2012 (UTC)

: It can be used directly in the formula there for calculating a determinant using the sign and the permutations. (But, from the talk page, you don't have to use any particular algorithm) --[[User:Paddy3118|Paddy3118]] 15:56, 30 July 2012 (UTC)

== Python code issues ==

Several of them:
# The whole thing makes no sense. You don't want a O(n!) algorithm for determinant when there's a polynomial one, so it's odd one should bear matrices in mind, especally when dealing with a slow language like Python.  Try a say, 12x12 matrix, and watch yourself die of old age before it finishes.
# Saving code here as spermutations.py and run the code in [[Matrix arithmetic]], it will say "TypeError: 'int' object is not iterable" (python 2) or "if DEBUG: print ' #', p SyntaxError" (python 3).  Disregarding the latter, probably line 19 in code over there should have said <code>s = list(spermutations(r))</code>?
# Change that, and now code blows up on line 35 in spermutations.py.  The 'n' here should be 'n1', maybe?
# Change ''that'', now <code>det([ range(5) for _ in range(5) ])</code> doesn't seem to return, ever. Maybe it's just Python being slow? (no not really).
# Replace the main section of spermutations.py with <code>for x in spermutations(range(3)): print(x)</code>, and run it. How many lines should it produce?
--[[User:Ledrug|Ledrug]] 19:27, 4 August 2012 (UTC)

:Hi Ledrug,
:# Its straight forward to see how that particular equation for a determinant is derived. That way of defining what a determinant ''is'' is still in use. Just like people are told about efficiency issues with the recursive method of calculating a factorial, but the recursive method of calculating a factorial has other legit uses. Computational efficiency has its place but is not all, and yes I do know that that particular method of calculating a determinant is inefficient. I did not say otherwise. 
:# And the others: My original versions of both programs worked together (Python 2.7). There have been edits since, I'll check (probably within the next day) - Thanks.
:--[[User:Paddy3118|Paddy3118]] 21:08, 4 August 2012 (UTC)
:: No it's not about making this method the most efficient for matrices, it's about why mentioning matrix -- or anything -- at all.  You have four things in this task: generate all permutations; list parity for each; use swap only; apply to matrices.  The first two are interesting enough, but there's no reason they can't be just included in the existing [[Permutations]] task, with or without the swapping requirement; the determinant thing is a nobrainer given the first two, particularly since it's not even a good method.  To make it worse, it's questionable if the code under "recursive" heading can even be called a swapping method.  Basically, I don't see why this task merits being a, well, task, instead of just a subheading under [[Permutations]]. --[[User:Ledrug|Ledrug]]

::We differ. That can be good. Try looking a little deeper and seeing if the author might not be trying to pull a fast one on you and cut them some slack. Or not. --[[User:Paddy3118|Paddy3118]] 03:17, 5 August 2012 (UTC)

:Yep Ledrug, although its use in the other task is mentioned, somone made a well-meaning change to the functions interfacethat broke things. I have removed that change as it was quickest, but I should probably update the Matrix task in the future. --[[User:Paddy3118|Paddy3118]] 21:45, 4 August 2012 (UTC)
