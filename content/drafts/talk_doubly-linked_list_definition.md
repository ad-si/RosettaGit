+++
title = "Talk:Doubly-linked list/Definition"
description = ""
date = 2017-06-16T01:59:13Z
aliases = []
[extra]
id = 3276
[taxonomies]
categories = []
tags = []
+++

Some languages provide pre-defined libraries for doubly-linked lists. Do you want to allow use of those libraries? --[[User:Waldorf|Waldorf]] 21:44, 26 December 2008 (UTC)

: I would say showing implementation would be more interesting than simply using "vectors", "lists" or whatever else the language/built in libs have. Nonetheless, as it happened in another task, this site is about showing common (?) way to do things in a language, not implementing libraries... So if a (standard and widespread) libs/way exist in a language, I believe it should be used. Am I right? ... In C, there's no such a standard library (or I don't know it...), so I implemented the code. --[[User:ShinTakezou|ShinTakezou]] 18:17, 13 January 2009 (UTC)


### About C

I wrote this code for another task, but it was not suitable for it. Then I discovered it is suitable for this one; but I did not test it deeply. --[[User:ShinTakezou|ShinTakezou]] 18:17, 13 January 2009 (UTC)

==Relation with other tasks?==
There two colliding tasks:
*[[Doubly-Linked List (element)]]
*[[Doubly-Linked List (element insertion)]]
This seems like a piece of silly duplication... —[[User:Dkf|Dkf]] 13:06, 16 May 2009 (UTC)

:Yes. Feel free to merge.  Same with the Singly-linked list tasks. --[[User:IanOsgood|IanOsgood]] 14:26, 16 May 2009 (UTC)

==Task definition is unclear?==
What does it mean "The structure should not allow circular loops"? To me, it's the design of data structure, which should support that; however, I seem to see in the some examples that it's the attached code which takes care about that.
: I also want to know. Is this requirement intended to rule out circular implementations of the doubly linked lists which use a "sentinel node" to eliminate corner cases? [[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 03:34, 16 October 2016 (UTC)
: Note also that a doubly-linked list is inherently circular. Between every node and its predecessor, if it has one, there is a circular relationship: the node points to its predecessor and its predecessor points to the node. That is a reference cycle.  Ergo, this task description is flawed: a doubly-linked list without "circular loops" is a direct contradiction. Maybe the intent is to say the operations on the list must check for corruption? Such that if a node is to be added to a doubly-linked list, the operation must fail/assert/throw if node is already entangled in a doubly linked list? [[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 15:33, 16 October 2016 (UTC)

And the code itself, by the way, isn't required by this task, though it would clearly be useful to have data structures together with associated code for working with them. Should we update the task definition (and possibly make existing examples invalid)?[[User:Avmich|Avmich]] 01:36, 12 November 2009 (UTC)

I'm bumping this. Are built-in libraries allowed? I agree that "circular loops" are just part of the design and should be allowed. This needs to be refined. --[[User:Mwn3d|Mwn3d]] 20:46, 9 February 2010 (UTC)
: This is one of the original tasks I created when I first launched Rosetta Code. I didn't even know of C++'s std::list at the time. For all I care, it (and its relations) can be deprecated and replaced with somet. In fact, I'd recommend that.  One good alternate direction would be to seek to catalog and demonstrate all common container structures, in both library and transparent-implementation form. (Much like we have under [[:Category:Control Structures]], but perhaps a bit more ambitious with the transparent-implementation goal.) --[[User:Short Circuit|Michael Mol]] 05:57, 10 February 2010 (UTC)

== Python attitude problem ==

The Python section is plain wrong.  Linked list is for algorithms, not just normal data storage, saying "I'm high level language la la la" is just silly.
:Hi Ledrug. Please sign your comments on talk pages. 
:I disagree with your statement. An interpreted linked list ''is'' going to be much slower, harder to maintain, and a needless source of bugs when compared to Pythons in-built list data-structure. The in-built list has all the interface of a doubly-linked list, and any interpreted version would not  work any faster or be any cleaner in an algorithm calling for (doubly) linked lists that were programmed in Python. The reason Python has lists is for them to be used. 
:[[Tree traversal#Python]] takes a different tack and does show the use of named_tuples in creating a tree of data, but then that task is more than "show the datastructure". 
:Of course, one could write a class implementing a doubly linked list in interpreted Python and append it to the Python section of the task, but I think my comment would still hold. --[[User:Paddy3118|Paddy3118]] 07:01, 10 June 2011 (UTC)
::I said, linnked list is for algorithms, not everyday data storage.  The value of linked list is that connectivity info is stored on the elements, not the container.  What if your job is sorting thought a set of nodes and connectivity rules, and separate nodes or edges into a set of lists to begin with?  When you have only local connectivity infomation to work on, you could either use a linked list, or look up who's connected to whom in a dictionary--which is the samething really, the point is: you don't have a nicely indexed list to use yet.  It's not like C people prefer linked lists over consecutively indexed arrays if it were possible, but sometimes complexity or efficiency simply won't allow it. -- [[User:Ledrug|Ledrug]] 21:33, 10 June 2011 (UTC)
::: Was this a question about problems like [[Resistor mesh]]? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]])
::: I completely agree with Ledrug. The Python list is a container; it doesn't address problems that require the graph properties of a list. You can't even get a handle on a list node and ask, what is the successor? A solution to this task is possible in Python; the pontificating text currently in its place is a cop out.[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 01:59, 16 June 2017 (UTC)
