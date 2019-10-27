+++
title = "Talk:Singly-linked list/Traversal"
description = ""
date = 2010-02-06T14:56:19Z
aliases = []
[extra]
id = 4809
[taxonomies]
categories = []
tags = []
+++

==J implementation==

The J implementation presented here is useless, from a J programmer's point of view, since it does not construct a meaningful result and because J programmers already have faster and more concise tools for dealing with sequences.

In other words:

* The task set here does not specify a meaningful result.
* I can imagine several different kinds of meaningful results which would fit this task.
* The underlying concept (a singly linked list) is really a language-specific optimization of a more general concept (sequences) and this "optimization" is, at best, irrelevant for J.

From the point of view of a programmer in another language, this might seem silly -- if you can not efficiently update a singly linked list, how can you scale up and deal with a lot of updates?  However, a J programmer might treat a large collection of updates as a collection (all updates which need to be dealt with in this unit of time), and apply them all simultaneously.  Here, you would need to architect your program differently than you would for doing lots of updates independently -- and a singly linked list is probably the wrong data structure for that task.

More generally (and this applies to all languages), good programmers push back when they get specifications which require specific technologies instead of useful results.  They might wind up using those technologies anyways (and legacy systems may often dictate the technologies used).  But technology belongs in the implementation, or related places such as in informal working notes, and does not belong in the specification.

[[User:Rdm|Rdm]] 12:52, 1 September 2009 (UTC)

Rdm, I think a better solution for this task would be just tell that verbs in J automatically consume all the input - either in chunks, if the verb rank is smaller than the input rank, or as a whole.

The task doesn't require "traverse sequentially", and J doesn't guarantee the order of traversal. So, to just traverse, say, array arr1 with monad verb1 one just applies that monad: verb1 arr1 . The sequential traversal - or a traversal where order does matter - are both separate cases in J, and should be noted differently.

[[User:Avmich|Avmich]] 23:07, 14 September 2009 (UTC)
