+++
title = "Talk:Doubly-linked list/Element definition"
description = ""
date = 2010-02-06T13:11:00Z
aliases = []
[extra]
id = 2920
[taxonomies]
categories = []
tags = []
+++

The Ada 2005 version includes links Next and Previous with the Not Null attributes. How does one designate either end of the doubly linked list without the null link? Another way to ask is, what does the previous link access at the head of the list, and what does the Next link access at the end of the list? --[[User:Waldorf|Waldorf]] 22:56, 18 July 2008 (UTC)

:Actually, the pointers should never be invalid in a doubly-linked list element. The idea is that the head of the list, if any, is indicated by an element rather than by a state of. This is a great advantage of doubly-linked lists. There is no need to mark the ends, because the operations of removal and insertion in fact do not require the list head. In a circular list these operations become more efficient. Further, insertion can be written in a way that it will automatically remove the inserted element from its previous list. To summarize, the advantage is that the elements do not know their lists.

:Well, this might become tricky if empty list as an object is needed. In that case to keep a pointer to the first element would of course kill the idea. A technique to handle this is to have a special head element, which does not contain any data and is never visited when data has to be accessed. Of course there there are problems too. One well known problem is a need of downcasting in an OO design. It can solved by making head element implementing the interface of other nodes with null operations, as they never called anyway. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:24, 19 July 2008 (UTC)



### Doubly linked list

In my experience on the AmigaOS, doubly linked list are implemented in a rather different way: there's a List header that also works like a special ''mark the end element'' of the list (implemented considering the List header like a partial superposition of two nodes, one ''shifted'' with respect to the other); so the list can be traversed in both directions, we know which is the first and the last element, and we can add fastly nodes to the head '''and''' to the tail in a rather smart and fast way. I have implemented the code (anyway it surely can be found somewhere, done better: normally it is implemented as macros, I've used functions), but I suppose that creating a new task for that will clashes with this one.  --[[User:ShinTakezou|ShinTakezou]] 23:57, 8 December 2008 (UTC)
:That sounds like a double ended queue to me rather than just a linked list. I may be wrong though. I haven't looked into efficiently implementing collections much since most languages just have them already. --[[User:Mwn3d|Mwn3d]] 03:17, 9 December 2008 (UTC)
::I think I haven't explained it very well. Label (list, queue... I don't know which applies to the case) apart, the implementation only allow you to know which is the head and the tail (only one end, and one start) and to be able to add fastly a node to the end of the list or to its head. With a ''simply'' linked list like the one here considered, what happens is that one should traverse the whole list to reach the end... or reserve some extra space in some variable to hold the last node of the list... but then, it would be better to use the implementation I had in mind. Anyway, the code in there on my hd (indeed... not too much tested:D), if needed I can put it somewhere. --[[User:ShinTakezou|ShinTakezou]] 01:06, 10 December 2008 (UTC)
