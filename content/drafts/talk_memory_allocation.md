+++
title = "Talk:Memory allocation"
description = ""
date = 2019-08-20T14:38:58Z
aliases = []
[extra]
id = 4249
[taxonomies]
categories = []
tags = []
+++

== languages like C ... ==
About langs like C, the task requires explanation about the fact that local variables are "allocated" on the stack (likely), so that a way of "allocating" 100 integers is simply <code>int ints[100]</code> ... even though this is not an ''explicit allocation''? --[[User:ShinTakezou|ShinTakezou]] 16:51, 26 May 2009 (UTC)
: Yes, please do explain the difference in lifetime and syntax of "auto" vs. malloc() allocations. --[[User:IanOsgood|IanOsgood]] 16:53, 26 May 2009 (UTC)
:: POSIX should have also shmget and friends for shared memory (shared among processes, not threads)... but sincerely I've never used them :D --[[User:ShinTakezou|ShinTakezou]] 17:39, 26 May 2009 (UTC)

== Task Applicability ==

I think this task does not apply to [[Tcl]], and suspect it is not applicable to any other dynamic language either. They all leave memory management to their runtime; that's part of their charm. But I won't help them by marking tasks with <nowiki>{{Omit}}</nowiki> for them... —[[User:Dkf|Dkf]] 19:11, 26 May 2009 (UTC)
: I was hoping that even dynamic languages could show how to how to manage lifetimes of blocks of memory, for use in foreign function interfaces, for example. --[[User:IanOsgood|IanOsgood]] 22:18, 26 May 2009 (UTC)
:: That tends to be handled in the FFI binding itself. I do have some ideas on how to do this, but it needs quite a bit of code. —12:54, 11 June 2009 (UTC)

I am not clear on what exactly counts as allocating a "block" of memory. Does it mean you have to explicitly calculate the size of the memory you are allocating? or does allocating an object count? or allocating an array?

Because if you take the C example codes using malloc() and all, the equivalent way to do the things in C++ would be to use "new" and "new []", e.g. "new int" (allocates space for one int), "new int[10]" (allocates space for 10 ints), "new MyClass" (allocates space for one MyClass object, and initializes it at the same time). Notice that you don't have to manually calculate the sizes -- they are calculated for you from the type. So do these count for this task? Then you can take these a step further -- the last two of them can be translated into Java: "new int[10]" (allocates an array of 10 ints), "new MyClass()" (allocates space for one MyClass object, and initializes it at the same time). Do these Java allocations fall under this task too? --[[User:Spoon!|Spoon!]] 04:57, 27 May 2009 (UTC)

: Details: in C (so to say, "low level", but not so low after all), you don't need to know the size of an "object", you can ask for it with the sizeof operator. When declaring a variable like "int c[10]", this is implicit; when using calloc, you say the size of the block, and how many blocks (sizeof(type) and 10). You could never need to say explicitly something like "11 * 10" (11 size of the "thing" and 10 how many), it is just an option; people can always use structs and simply use "sizeof" if they need their size... but aside if you want a block of 1439 bytes (from the heap) without any special "meaning", you can say simply malloc(1439). More "abstract" langs maybe say it differently, but they always have to have a way of "reserving" a number of bytes from "somewhere". I think this task is about saying how it can be done (the common way too), where the room is taken (hard to say for langs like Python... should see their internals!?), and for how long it last before it is no more usable. 
: It could be also possible it is not suitable for many "high level langs", but this is not a surprise: most of the more complex functional or OO paradigm tasks are not suitable for C or assembly (even if implementable of course)... --[[User:ShinTakezou|ShinTakezou]] 16:21, 27 May 2009 (UTC)
