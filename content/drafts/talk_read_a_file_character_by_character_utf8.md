+++
title = "Talk:Read a file character by character/UTF8"
description = ""
date = 2014-06-25T08:54:05Z
aliases = []
[extra]
id = 17539
[taxonomies]
categories = []
tags = []
+++

== Why? ==

What is the point here?

Specifically: are we trying to disable operating system buffers? Are we trying to disable language-maintained buffers? Do we actually not care if buffers are used? Is this about using sequential file reads to accomplish inter-process communication? (Are we thinking that an indexed file read can simulate the use of a device file?)

For example, let us say we memory map a file - does that count as "reading" the file? Or can we claim that the instructions access the mapped memory represent the "character read"?

(I am inclined to implement this as an abstraction layer on top of "reading the entire file", but wanted to make sure I was clear on the purpose before attempting the implementation.)

:: The task description doesn't say anything about implementation details; I think the point is just to demonstrate an '''interface''' that allows programmers to repeatedly get the next character of a file, which can be useful for certain algorithms. Many programming languages have a built-in function for this (or at least easy access to a library / system call that provides it), so it's not an esoteric problem. What goes on behind the scenes to implement this interface probably doesn't matter much, although I suppose it should be a "reasonable" way to implement it (for practical purposes), and of course better performance is always better. At least that's my interpretation. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 08:54, 25 June 2014 (UTC)
