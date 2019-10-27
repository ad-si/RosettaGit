+++
title = "Talk:Stack"
description = ""
date = 2011-09-15T19:07:57Z
aliases = []
[extra]
id = 1971
[taxonomies]
categories = []
tags = []
+++

== ... is this really a task? ==
Not sure whether this is the right place to bring this up, but is this really a task? I mean: nothing is being done here. If the intended task is "implement a stack" then the reference to objects already implies a particular type of implementation (one that precludes functional languages, for example). But if it is really about objects, then isn't the task here really just "instantiate something from your library"?  [[User:Sgeier|Sgeier]] 00:43, 24 February 2007 (EST)
:I'm not sure what the purpose of the task is.  The guy who created it wasn't logged in, so I don't know how we can ask.  I'll throw a clarification template on it and take it out of the tasks category until it's been clarified.  Also, the format isn't standard.  It's interesting, but I think there are better ways to clarify the page layout than using the messagebox CSS class for the description.
:
:Oh, and this is the perfect place to bring this up. --[[User:Short Circuit|Short Circuit]] 10:32, 26 February 2007 (EST)

: Ignoring the matter of whether an object oriented approach is required (or whether non-OO languages are precluded from this task) I would point out that many programming courses introduce stacks as a basic data structure and use the implementation of a stack with "push", "pop" and "depth" functions as a programming exercise.  Granted that languages such as Python, Perl, Lisp/Scheme and Lua will make this a trivial program (but no more so than "Hello, World" which we also include as a task on this site).  I think it's a fair task but perhaps the wording could be cleaned up.[[User:JimD|JimD]] 21:37, 1 November 2007 (MDT)
: Additionally, as JimD said, a stack is a data structure. Shouldn't it also be in the data structures category? --[[User:mwn3d|mwn3d]] 18:37, 9 November 2007 (EST)


### Moved

Moved this to data structures. --[[User:Short Circuit|Short Circuit]] 14:23, 10 November 2007 (MST)

== Deleted paragraph ==
The task formerly contained this paragraph: ''"Stacks as a containers presume copyable elements. I.e. stack elements have by-value semantics. This means that when an element is pushed onto the stack, a new instance of the element's type is created. This instance has a value equivalent to one the pushed element."''

I deleted this paragraph because the code ignored it. The implementations for Common Lisp, Factor, and Ruby copy references to objects; they never make "a new instance of the element's type". In general, stacks never presume to make such new instances. --[[User:Kernigh|Kernigh]] 19:07, 15 September 2011 (UTC)
