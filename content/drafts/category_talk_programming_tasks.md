+++
title = "Category talk:Programming Tasks"
description = ""
date = 2019-02-05T12:07:41Z
aliases = []
[extra]
id = 1550
[taxonomies]
categories = []
tags = []
+++

=New Programming Tasks=
If you have an idea for a programming task, mention it  [[Help:Request a new programming task|here]]; we'll help you work it out and get it posted. --[[User:Short Circuit|Short Circuit]] 08:44, 11 January 2007 (PST)

== Why have both this category and [[:Category:Solutions by Programming Task]]? ==

Surely just one category is sufficient? --[[User:IanOsgood|IanOsgood]] 11:25, 13 September 2007 (MDT)
:Seconded --[[User:Mwn3d|Mwn3d]] 23:26, 4 December 2007 (MST)
:I don't know why, for certain.  I did notice that not all of the tasks in this category are in Solutions by Programming Task, though. I prefer the "Solutions by" naming scheme, myself. --[[User:Short Circuit|Short Circuit]] 09:32, 5 December 2007 (MST)
:: There's value to both; one is just a collection of all tasks, and the other is organized more by theme. –[[User:Dkf|Donal Fellows]] 16:30, 4 September 2010 (UTC)

I agree with Donal: Both lists are useful. One is alphabetical, the other is grouped. We just need a link to the categorized list in the left hand column now. I suggest we have "By Category" under the word "Tasks" in the left hand column.

[[User:Markhobley|Markhobley]] 10:27, 29 May 2011 (UTC)

== Sort programming tasks ==
We really need to have the ability to sort the programming tasks not only alphabetically but also chronologically. As it stands now, it's almost impossible to know which programming tasks have been added (or updated) recently. It's also difficult to know whether a task you have suggested has been accepted and added as an 'official' task for others to start solving.
[[User:Francogrex|Francogrex]] 8:05, 06 October 2011 (UTC)
:To solve the first problem we have [http://rosettacode.org/wiki/Special:Ask/-5B-5BIs-20task::true-5D-5D/order%3DDESC/sort%3DModification_date this link] from the main page. For the other one I just watch [[Special:RecentChanges]] to see when a task gets created. You can get an RSS feed for that or just reload it all the time (which is what I do). --[[User:Mwn3d|Mwn3d]] 14:08, 7 October 2011 (UTC)

== Are there any Recursion Tasks? ==

I was hoping to find some recursion examples. Because some languages cannot perform recursion, I suppose that is enough reason not to include them, but if I just missed it I would like to know where these tasks could be found.
Thanks --[[User:Rlrandallx|Rlrandallx]] 10:52, 23 February 2012
:Here they are: [[:Category:Recursion]]. --[[User:Mwn3d|Mwn3d]] 19:56, 23 February 2012 (UTC)

== Return the string to the left or right of a search string ==

I have a whole series of string manipulation functions I have written in C# and Objective-C that search within a string to find a substring and return the content to the left, right or between the occurrence of the search string.

How do I add my code example and get these included in the list so others can contribute their versions in the other languages?

== Singly-linked list/Arbitrary Number Multiplication ==

This program aims to multiply 2 arbitrary-length decimal numbers and returns the output in decimal number format by using linked list structure.

== Producer-Consumer Pattern

This task demonstrates examples of the producer-consumer pattern in the chosen language. There are several variations on the producer-consumer pattern including synchronous, asynchronous, bounded queue, and unbounded queue. The simplest implementation of a producer-consumer has one producing task or thread and one consuming task or thread. More complex implementations may have 1..P producers and 1..C consumers. The number of producers and consumers need not be the same. A producer-consumer implementation may require that every data item produced by the producer(s) must be consumed exactly once by one of the consumers, or it may require that the consumer(s) only sample the data produced by the producer(s). An example of a sampling application might be an automated weather station. The weather station may produce temperature readings at a rate of 100Hz while the consumer displaying those readings on a website may only display changes at a rate of 1Hz. The consumer only needs to display one reading per second and can ignore 99 readings every second.
