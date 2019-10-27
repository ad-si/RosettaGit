+++
title = "Talk:Inheritance/Single/C"
description = ""
date = 2010-02-04T15:33:59Z
aliases = []
[extra]
id = 5336
[taxonomies]
categories = []
tags = []
+++

== OO? ==
First, I am not trying to be flippant here, obviously this C entry is a lot of work. 

The task seems to me to only truly apply to object oriented languages. Would it not be better to simply state that C is not OO? 

I was going to consider the following which would allow the inclusion of the C code 

''"Although C is not considered an OO language, you can do the following ..."''.

But knowing that C++ was designed to be both OO and C backward compatable (to some degree); and noting the brevity of the [[Inheritance#C++|C++ entry]] compared to this C one, I can't see anyone actually doing something like this in C except to show that it can be done. Please, please don't get mad - it is hard to convey tone when writing. All I am trying to do is solicit comments on whether the current C entry is wise. (If I had gone to so much effort, I would have to think twice about someone suggesting it be removed). --[[User:Paddy3118|Paddy3118]] 03:34, 17 January 2010 (UTC)

<small>(P.S. Someone had noted how often people started out by saying "With all respect ..." then continued in a disrespectful tone. I have tried hard not to do that).</small>

: I understand.  I thought I'd do this more or less for pedagogical(sp?) purposes.  Some comment about C not being a OO language is certainly appropriate. If you're familiar with the X window system, you'll see some similarities between what's in the XtIntrinsic library and some of this code. That was all a C based Object system, much more complete than this.  --[[User:Rldrenth|Rldrenth]] 05:46, 17 January 2010 (UTC)

: (My opinion) The "Unimplemented task" pages specifically list tasks marked as omitted, yet encourages people to try to implement them. Proofs of concepts such can illustrate where a problem may be solveable using a language, yet be particularly unidiomatic. In short, it can show how you might need to do something if you really have to, but also serve as a warning suggesting there may be a better way. In short, I think the ''example'' is wise, but as an example, not a recommendation. --[[User:Short Circuit|Michael Mol]] 06:34, 17 January 2010 (UTC)

: Actually, there is a full object system for plain C: [http://library.gnome.org/devel/gobject/unstable/pr01.html GObject]. In fact, this implementation might be fruitfully replaced with one that uses GObject. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 12:26, 17 January 2010 (UTC)

:: That would probably be a good Idea.  With what's here, the main purpose gets rather lost in the haystack.--[[User:Rldrenth|Rldrenth]] 13:40, 17 January 2010 (UTC)
::: +1 --[[User:Paddy3118|Paddy3118]] 16:59, 17 January 2010 (UTC)
:: I would argue against replacing the example wholesale, but I wouldn't mind seeing an additional example using the glibc library. It becomes a comparison of "roll-your-own" versus using a library.  ''Eventually'', changes the site's organization (and, likely, software) will make multiple side-by-side examples in the same language less clunky. --[[User:Short Circuit|Michael Mol]] 20:24, 17 January 2010 (UTC)
::: An example based on an existing library would be clearer to most readers.  One could then put a link on that example to this one, and state that this one would provide some information about how the libraries are implemented for those with the diligence to wade through considerable code.  Truth is, this example will not be useful to most readers, but it may provide some insight to a few of the curious. --[[User:Rldrenth|Rldrenth]] 22:32, 17 January 2010 (UTC)

:You'd be amazed at how many object systems have been implemented in C and other mostly procedural languages. OO is a useful enough design pattern that whole languages have been built around it, after all. In C, the implementer gets to choose ''how'' to implement it, instead of relying on the particular implementation built into the language. That's one reason we have Objective-C vs. C++.  For example, the OO system built into the heart of Unix; what else is a device but a couple of classes (character, block) designed to be polymorphic on the open/close/read/write/ioctl interface, implemented as a couple big vtables? Most of the ad-hoc OO systems I've seen don't have general inheritance, relying instead on explicit delegation. --[[User:IanOsgood|IanOsgood]] 18:35, 17 January 2010 (UTC)

:: Is there a C OO library that is most popular that could be used for this task then? I too would prefer we used an existing library, but I would also prefer a library that was prominent within the C community and "general purpose". --[[User:Paddy3118|Paddy3118]] 06:02, 18 January 2010 (UTC)

::: It would be good to show off how to do it with glib (as noted above) and Xt (horrible, but still existing) as additional sections. If there are others, they can be added too; no reason why not! –[[User:Dkf|Donal Fellows]] 10:04, 18 January 2010 (UTC)
:::: +1. I'll go a step farther and point out that some C examples might be small enough to not require breaking out into a subpage, bringing more content into immediate view. --[[User:Short Circuit|Michael Mol]] 15:01, 18 January 2010 (UTC)
::::: I doubt if Xt would fall into that category, though. IIRC, its even more of a task to subclass than the example seen here. --[[User:Rldrenth|Rldrenth]] 16:01, 18 January 2010 (UTC)
