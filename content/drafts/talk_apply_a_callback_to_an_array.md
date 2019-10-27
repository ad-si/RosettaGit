+++
title = "Talk:Apply a callback to an array"
description = ""
date = 2017-06-08T13:38:21Z
aliases = []
[extra]
id = 1674
[taxonomies]
categories = []
tags = []
+++

==properly defining task==
The problem is, of course, that they're all doing slightly different things.  To some extent this is unavoidable due to language differences, but when one example is mapping the results into another array and a different example decides to print them, that's just a bit sloppy.  Though I certainly don't mind if you have secondary examples demonstrating different techniques or modifications to the basic premise (such as the c++ use of a binary function).

:I wrote up some [[Help:Common Issues|guidelines]], but I need to make them more visible.  I'll probably add them to--or link to them from--the programming task template.--[[User:Short Circuit|Short Circuit]] 12:46, 23 January 2007 (EST)

::The problem is that the task is not properly defined. There is the same problem with most of the tasks in Rosetta Code. I don't understand why it is so difficult to write even a few sentences to describe what to be done. Writing the specifications is the most importand part of any sofware project!
::--[[User:PauliKL|PauliKL]] 17:42, 28 August 2008 (UTC)

== array vs list ==

I was just looking at the [[Haskell]] solution for this task, and it uses a list instead of an array.  Of course, Haskell has built in arrays, lists are just more natural, as they are in all functional languages.  This seems deceptive to me.  I know lists are more natural than arrays in Haskell, but it seems to me this page should contain the array example, and an alternate task should be made for lists.  Just as Haskell will seem more complicated on the array examples, imperative languages like [[C]] will have to use a complicated list structure for the list examples.  It doesn't seem right to neglect this distinction. Thoughts? --[[User:Zarvok | Zarvok]] | [[User_talk:Zarvok|Talk]] 22:45, 23 January 2007 (EST)

:I don't know any functional languages.  What's the difference between a list and an array? --[[User:Short Circuit|Short Circuit]] 23:27, 23 January 2007 (EST)

::TCL is almost-functional (it steals a lot from LISP). I'll put an example for either in there. A list is just what the name implies -- essentially what would be called a one-dimensional array in many other languages. It is the natural storage container for a bunch of data in TCL (and indeed was the only data structure for a long time). In TCL, at least, the notion of an "array" strictly implies an <i>associate</i> array. I.e. looping over a list means looping over the elements of the list (and doing something with them), but looping over an array means looping over the <i>keys</i> of the array (and doing something with the elements associated with those keys). [[User:Sgeier|Sgeier]] 18:28, 1 February 2007 (EST)

:::I still struggle with this differentiation, and what it may imply when providing examples in [[J]], which is one of the languages where "a list" means "an array of one axis (or dimension)". If "list" means a unit of data that lacks any key-reference aspect, then perhaps tasks that specify use of lists should not be completed using J? --[[User:TBH|TBH]] 16:36, 15 January 2008 (MST)

::::My opinion: I interpret this particular task as "Apply a callback to a '''collection'''". The array-ness is secondary, it is the callback that is of interest. --[[User:IanOsgood|IanOsgood]] 17:11, 15 January 2008 (MST)
:::::Did anybody notice there's no task description?  This can all be wrapped with a clarification.  Feel free to clean it up. --[[User:Short Circuit|Short Circuit]] 21:14, 15 January 2008 (MST)
::::::I did add a task description.  I'm concerned that it may not be specific enough to qualify as a callback.  I didn't use the term "collection", as that would have special meaning in VB.Net and (I think) Java. --[[User:Short Circuit|Short Circuit]] 00:19, 17 January 2008 (MST)
:::::I agree that the callback aspect is the gist of this task. This place might not be the best for me to have raised my concern. Still, it seems that the list-vs-array specification is part of a pattern of problem that occurs for some languages, under some tasks, which we may want to overcome by refinement of task descriptions. --[[User:TBH|TBH]] 21:25, 15 January 2008 (MST)
::::::That was my interpretation of the intent as well.--[[User:Short Circuit|Short Circuit]] 00:19, 17 January 2008 (MST)

== specifying callback ==

The opening sentence of the Wikipedia [http://en.wikipedia.org/wiki/Callback_%28computer_science%29 entry] is: "In computer programming, a callback is executable code that is passed as an argument to other code." The current task description does not seem to specify that a callback is to be used. ("In this task, the goal is to take a combined set of elements and apply a function to each element.") Just adding the word to this won't work, as we wish to communicate what counts as a callback. --[[User:TBH|TBH]] 07:10, 16 January 2008 (MST)
:I was never really clear on what a callback is. Is it like a composition operator? --[[User:Mwn3d|Mwn3d]] 19:01, 16 January 2008 (MST)
::I see callbacks and function composition as only weakly related for this purpose.  Lambda notation always involves function composition, and its functions are always anonymous. I think function anonymity is not crucial to callbacks, but that it can be involved with some means of accomplishing it. --[[User:TBH|TBH]] 13:10, 18 January 2008 (MST)
:The task description needs to be changed.  A lot of the code found here would be better placed in [[Loop Structures]]--but that task is currently being reconstructed by [[User:Marshmallows|Marshmallows]], so the task rewrite and review should probably wait until Marshmallows is done.  At that point, though, this task should probably also be given a clearer name.  I was thinking [[Apply a callback to members of a collection]].  But that might be too verbose.  Thoughts? --[[User:Short Circuit|Short Circuit]] 23:30, 10 April 2008 (MDT)
As I understand it: A callback is characterized by passing executable code as an argument to other code. Anonymity is characterized by the absence of binding between a function and a name. --[[User:TBH|TBH]] 12:39, 18 January 2008 (MST)

== VB.NET ==

I removed the VB code because it doesn't work. Console.WriteLine doesn't return a value, and this cannot be used as an anonymous function.

== Problem Definition ==

Looking at the early history of this task, it seems that by "array" it is intended that "ordered sequence of values" be used and that "callback" means "run a bit of code" or maybe "apply a lambda term". Moreover, it's also clear that there is no need to update the sequence of values with the results of applying the lambda to each value. –[[User:Dkf|Donal Fellows]] 10:33, 4 January 2010 (UTC)
