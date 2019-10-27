+++
title = "Talk:Sorting algorithms/Tree sort on a linked list"
description = ""
date = 2019-03-19T17:12:20Z
aliases = []
[extra]
id = 17479
[taxonomies]
categories = []
tags = []
+++

==I don't understand this task==
I don't understand this task. Tree sort traditionally means loading the elements into a (new) binary search tree data structure, and then reading the elements back out. I don't understand what it means to "tree sort them inplace" in a linked list. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 03:08, 4 April 2014 (UTC)

: The Ching-Kuang Shene paper linked to in the task mentions having an existing doubly linked list that is temporarily reused to build up a new tree via insertion sort. Reused in the sense that while building the tree the existing <code>prev</code> and <code>next</code> node pointers are used as if they were <code>left</code> and <code>right</code> node pointers to the sub-trees. Once the tree is assembled it is straight forward to traverse the tree in-order setting the node pointers such that it is once again a doubly linked list but now sorted.
: &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 15:26, 12 July 2018 (UTC)

:: Ah, I knew there was a reason this had appeared on my radar. I propose replacing the "Test case" paragraph with
    Task:
    First, construct a doubly linked list (unsorted).
    Then construct a tree in situ: use the prev and next of that list as left and right tree pointers.
    Then traverse the tree, in order, and recreate a doubly linked list, again in situ, but of course now in sorted order.
:: In the process, removing all references to Finnegans wake. Phix version 2 now assumes that, and I made that edit. &mdash;[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 08:15, 5 November 2018 (UTC)
::: The revised task description seems sensible to me so I've removed the sentence about "not adding to the task" and added a Go entry on the above lines. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:11, 19 March 2019 (UTC)
The proposed task talks about performance. This is a Bad Idea because it's next to impossible to compare performance between systems (different CPU speeds, different memory bandwidths, different loading patterns, etc.) Talking about performance strongly encourages people to try to “optimise” their implementations, which tends to make them significantly less readable and less idiomatic. Finally, actually measuring performance fairly and accurately is hard; there are lies, damned lies, and benchmarks. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 10:49, 11 May 2014 (UTC)

: For tasks like this performance should not be asked for in terms of CPU time or clock time (and especially not "BogoMips") but in terms of countable operations required. E.g. the number of comparisons and swaps required, or the number of comparisons and pointer changes required.
: &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 15:26, 12 July 2018 (UTC)

And the proposed sample text is ridiculously long; there's also no meaningful output. (Just printing some metadata about the supposed performance is insufficient, as it does not check that things are correct. Wrong “solutions” can be incredibly fast.) This whole task needs major revision (and soon!) or I'll have to consider deleting it entirely. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 10:55, 11 May 2014 (UTC)

:Seconded. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:54, 11 May 2014 (UTC)
:Thirded.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:39, 30 October 2017 (UTC)

==why must the input be so much work to retrieve?==
Couldn't the author of this task download the whole she-bang (the complete text) and uploaded it to just   ''one''   file on Rosetta Code?   That would make it a lot simpler than to have each programmer (code writer) to go through all the work of downloading each of the four books and multiple chapters.   This is, I hope, a task to show a tree sort on a linked list, not a scavenger hunt (or quest).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:39, 30 October 2017 (UTC)
