+++
title = "Talk:External sort"
description = ""
date = 2019-06-15T17:59:18Z
aliases = []
[extra]
id = 21156
[taxonomies]
categories = []
tags = []
+++

== Is this task meaningful? ==

This kind of task made a lot of sense back when you might have had only 1k of memory and operating systems were primitive.

Nowadays, it's not uncommon to have 64GB of physical memory (or more), and operating systems that give us virtual memory, and we have tools like hadoop for when we have too much data for "one machine" to handle. So, this seems... not so relevant. Nor does it seem like a well defined task.

For example, the existence virtual memory alone suggests that we should be able to use a language's built-in sort to satisfy at least one interpretation of this task.

It's also something of a problem that there's no sample dataset for this task, and that there's no way of showing sample output for this task. The ambiguities of the task, and the physical limitations of this site, make such things daunting. How do we verify the accuracy of an implementation?

But a worse problem, for this site, is that it's not likely that approaches to this task will be very comparable across languages. And that's because this task seems to be more about the operating system and hardware than it is about the language. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:24, 10 October 2016 (UTC)

:I did once sort a list of a billion 64-bit integers by sorting the chunks and then merge-sort/combining the chunks. I would agree it's a pretty rare situation and it is probably a poor fit for this site's format. --[[User:TimSC|TimSC]] ([[User talk:TimSC|talk]]) 05:06, 18 December 2016 (UTC)

:: There are many times where it may <u>not</u> be a good idea to let the   ''sort''   process use all available real storage, especially when there may be many other critical programs running;   and letting the   ''sort''   thrash real storage   (in a paging sense)   would be detrimental to the system   (and all other running programs)   as a whole.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:02, 15 June 2019 (UTC)

::: Valid. I've experienced a bit too much of this lately. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:59, 15 June 2019 (UTC)
