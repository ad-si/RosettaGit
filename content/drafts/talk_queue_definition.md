+++
title = "Talk:Queue/Definition"
description = ""
date = 2015-11-21T21:29:56Z
aliases = []
[extra]
id = 2189
[taxonomies]
categories = []
tags = []
+++

==Requirement to define a linked list==
The last line of the task description states ''Define the data structure for a FIFO element. Said element should contain a data member capable of holding a numeric value, and the link to the next element should be mutable.'' The Python example first provided does not satisfy this requirement. It seems that the Python example should be expanded to include a definition of the data structure for a FIFO element.
- [[Waldorf|Waldorf]] 4 November 2007
: Python's dequeue looks like it implements generic programming; The data structure that would need to be defined is actually internal to the dequeue class.  In my opinion, languages which have (either through built-in features or public libraries) features that simplify the implementation of the task should have those features demonstrated in examples.  In this case, I would suggest exempting Python from the task requirement.  Considering the FIFO data structure is a fairly generic and common one, I would ask '''Nirs''' if that requirement is necessary, or if it could be reworded. --[[User:Short Circuit|Short Circuit]] 19:20, 4 November 2007 (MST)
::I did not add that requirement, and I think it is wrong. Each language should implement it in the most natural way for the language. --[[User:Nirs|Nirs]] 07:04, 5 November 2007 (MST)
:::Ah.  I should probably have looked at the history more carefully. --[[User:Short Circuit|Short Circuit]] 11:57, 5 November 2007 (MST)

* The Python example is usage of language/library features, whereas the Ada example is of implementation; these are both useful things to have. I suggest that the task be rewritten to allow for both implementation and usage examples, or that they be split into two task pages.
*: Usage examples are useful for "how to accomplish this in this language"; implementation examples are useful for "here's a program in this language" or "how to accomplish" if there is no library; both should have their places in Rosetta Code, and I think we should consider how to handle this problem in general. At the moment, I think that "FIFO (usage)" and "FIFO (implementation)" would be a good technique, with the usage examples pointing to the implementation examples when applicable.
*: If no one has any objection, I will split the pages in this manner, and contribute examples to both.
*: --[[User:Kevin Reid|Kevin Reid]] 17:46, 4 November 2007 (MST)
::I agree that both usage and implementation should get their own pages.  The same should probably go for the (x) Linked List and Stack tasks as well. --[[User:Short Circuit|Short Circuit]] 19:13, 4 November 2007 (MST)
::I don't see a need to split the page, specially when there is only two entries. Wait until other people add more entries and then we can see if there is a real problem here. --[[User:Nirs|Nirs]] 07:04, 5 November 2007 (MST)
:::I like the idea of having a FIFO (implementation) page, if only to show how a FIFO works at a low level.  Ditto other data structures. (Stack, linked lists, trees, etc.)  I was up until 2AM last night working on an animated GIF for that purpose.  I should have it finished tonight. --[[User:Short Circuit|Short Circuit]] 11:57, 5 November 2007 (MST)

Separate issue: Wouldn't it be better to not require a particular implementation strategy? Mutable links, for example, would be unnatural in certain languages. I propose that the requirement that "elements" be defined be removed. --[[User:Kevin Reid|Kevin Reid]] 17:46, 4 November 2007 (MST)
: It would probably be more appropriate to re-word the description like "If no appropriate data structure exists within the language or common libraries, define an appropriate data structure supporting (...)."  While FIFOs are common enough data structures that I would expect them to be possible in most languages, I would still ask '''Nirs''' if there was a reason he specified that the link be mutable. --[[User:Short Circuit|Short Circuit]] 19:13, 4 November 2007 (MST)
::I suggest to remove the requirement for fifo data structure. --[[User:Nirs|Nirs]] 07:04, 5 November 2007 (MST)
:::There is definitely no need to require some specific method to implement the FIFO. After all, in real world the implementation should be encapsulated anyway. And why does it require using linked list? That is ''not'' how FIFO is implemented in the real world. The most common implementation method is a circular buffer. --[[User:PauliKL|PauliKL]] 16:19, 19 November 2008 (UTC) 

==Illustration==
I created and added an illustration.  Hope you like it.  Gimp was ''not'' the right way to assemble an animated GIF. --[[User:Short Circuit|Short Circuit]] 23:32, 5 November 2007 (MST)

== Ada examples ==

The Ada section has actually four implementations, where two of them have the additional condition of being usable from concurrent threads. Since this makes the Ada section quite large and isn't actually necessary for the task, I propose to create a new task "concurrency-safe FIFO" or similar, and move the last two Ada examples there. --[[User:Ce|Ce]] 09:43, 28 February 2008 (MST)

== Merge? ==

It seems odd to me that [[Stack]]s need only 1 page, but Queues need 2.

I think this should be merged with [[Queue/Usage]] -- after all, ''just'' defining the queue doesn't necessarily illustrate how to use it, and Queue/Usage includes creating the queue as part of its description. The final merged page could then be located at [[Queue]], overwriting the current redirect. -- [[User:Eriksiers|Erik Siers]] 19:43, 2 February 2011 (UTC)

: [[Queue/Usage]] was ''split'' from this task at [http://rosettacode.org/mw/index.php?title=Queue/Usage&oldid=20617 5 January 2009]. This split was a bad idea. Queue/Definition and Queue/Usage should be one task. --[[User:Kernigh|Kernigh]] 01:07, 16 September 2011 (UTC)

The descriptions for these tasks are identical. They should be merged. [[User: crossrodas1112|crossrodas1112]] 16:28, 21 November 2015 (EST)
