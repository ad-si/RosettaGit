+++
title = "Talk:Sort disjoint sublist"
description = ""
date = 2011-02-15T00:36:10Z
aliases = []
[extra]
id = 9248
[taxonomies]
categories = []
tags = []
+++

Adapted from a question/answer [http://stackoverflow.com/questions/4962343/how-to-sort-a-disjoint-sublist/4976580#4976580 here]. Note that the solution for languages with pointers might be different than the Python as you may be able to adapt a sort routine to sort via an extra level of indirection. --[[User:Paddy3118|Paddy3118]] 06:34, 12 February 2011 (UTC)

:... Which has just been done by the Go example. Sweet! --[[User:Paddy3118|Paddy3118]] 06:02, 14 February 2011 (UTC)

:: I expect that swapping through a level of indirection to be less efficient than extracting the values, sorting them, then putting them back, for typical machine architectures and random sorts.  --[[User:Rdm|Rdm]] 12:58, 14 February 2011 (UTC)
::: I wouldn't want to call that either way; depends on too many factors (notably whether cache limits are observed). Only way to know is to measure in a realistic setting. –[[User:Dkf|Donal Fellows]] 16:42, 14 February 2011 (UTC)

== Indices as collection ==

I see that many languages take the indices as a general collection (or array) instead of specifically as a set. If they're doing that, should they also be enforcing uniqueness of the indices before progressing with the rest of the sort? (To be exact, failing to do this gives wrong answers…) –[[User:Dkf|Donal Fellows]] 16:45, 14 February 2011 (UTC)
:The task states that 6, 1, and 7 are given. I didn't want people to assume they got 1,6,7 in that order. I would therefore be inclined to not ''insist'' that a routine should also account for duplicates. (Although I had noted this addition in the TCL example which is fine). --[[User:Paddy3118|Paddy3118]] 17:01, 14 February 2011 (UTC)
::It seems like having duplicate indicies wouldn't change the outcome, but it would waste a few cycles reassigning the same number to the same index. some computer languages might not work like my brain though. --[[User:Mwn3d|Mwn3d]] 18:21, 14 February 2011 (UTC)
:::The problem is that the repeated indices will not, in general, correspond to the location of the repeated data, once the data is sorted.  --[[User:Rdm|Rdm]] 18:25, 14 February 2011 (UTC)
::::You're right. I tried the example on the main page using "6, 6, 1, 7" for the indices. Another solution would be to sort the indices after they come in. You could use the same sort function for that as you do for the data. Either way, it's an extra step. I vote for progrmamer's choice and let the indices come in however they will. --[[User:Mwn3d|Mwn3d]] 19:00, 14 February 2011 (UTC)
:::::I'm even more wrong. It's not good enough just to sort the indices. You should probably set-ify the indices as they come in to be sure. --[[User:Mwn3d|Mwn3d]] 21:58, 14 February 2011 (UTC)
:::::I confirmed that the Python-type algo's aren't upset by repeated terms - it just gives rise to extra, redundant work. You do, however have to eventually sort the indices and I think that this should be part of any correct answer that uses this type of algorithm rather than the Go-type algo. But even if duplicates did matter, I would read the task as ''not'' giving you duplicates. --[[User:Paddy3118|Paddy3118]] 19:49, 14 February 2011 (UTC)

::::::Except, python's algorithm can be upset by repeated terms:
::::::
```python
>>>
 def sort_disjoint_sublist(data, indices):
...     indices = sorted(indices)
...     values  = [data[i] for i in indices]
...     values.sort()
...     for index, value in zip(indices, values):
...             data[index] = value
...
>>> d = [7, 6, 5, 4, 3, 2, 1, 0]
>>> i = [6, 1, 7, 7]
>>> sort_disjoint_sublist(d, i)
>>> d
[7, 0, 5, 4, 3, 2, 0, 6]
```

::::::--[[User:Rdm|Rdm]] 20:05, 14 February 2011 (UTC)

::::::: Just goes to show... I tried [6,6,6,1,7], and [6,7,6,1,6,1,7] and just assumed ...
::::::: I'll tighten the task description. --[[User:Paddy3118|Paddy3118]] 00:36, 15 February 2011 (UTC)

:: I only asked because I couldn't see whether having duplicates and not filtering could guarantee a correct result or not. If I can't see why a property might hold, I suspect it doesn't (an application of the Paranoid Principle). It should be reasonably simple to fix by adding a unique-ing step to the processing of the index list. (Fun problem overall! Short and not quite as obvious as it looks at first glance.) –[[User:Dkf|Donal Fellows]] 00:10, 15 February 2011 (UTC)
