+++
title = "Talk:Sorting algorithms/Permutation sort"
description = ""
date = 2010-11-26T13:13:34Z
aliases = []
[extra]
id = 2854
[taxonomies]
categories = []
tags = []
+++

How is this different from [[Bogosort]]?
:I just changed the description to show how. Trying out permutations in any ordered fashion is not the same as randomly shuffling (which is what bogosort does).--[[User:Mwn3d|Mwn3d]] 19:13, 7 May 2008 (MDT)

==Tcl wrong?==
The Tcl example does not contain an explicit loop. It seems unlikely to me that "firstperm" is itself doing a permutation sort, and a web search didn't turn up anything informative (I don't know Tcl myself). Could someone who knows more add explanation/mark incorrect/fix? --[[User:Kevin Reid|Kevin Reid]] 23:40, 25 May 2009 (UTC)
:From [http://tcllib.sourceforge.net/doc/struct_list.html the docs] it seems that a solution has only just started to be etched out. How do you mark incorrect? --[[User:Paddy3118|Paddy3118]] 00:06, 26 May 2009 (UTC)
:OK, fixed --[[User:Glennj|glennj]] 00:55, 26 May 2009 (UTC)

==References & Disclaimer for Permutation Sort==
Given the O(n!) nature of this sort, I think it would probably be wise to post a warning on the page (ie DON'T DO THIS SORT!).  =)

Currently, I'm on travel and don't have access to my normal CS library.  Is there a discussion of this sort in any of the standard algorithm books?  Based on the problem description, I'm not sure what is or is not fair game with this sort.  For example, is iteratively checking and shuffling (ie a randomized approach) considered alright?  Is the real purpose to see if a language has built-in support for permutations?  

Also, when the task description isn't explicit about data structure support (eg list/array), I've noticed that most implementations only support arrays and a few implementations will support both data structures (or even a generic "sequence").  Is there a standard convention in place for noting when this occurs?  Obviously, generic support is much more desirable.  Just some thoughts...

: O(n!) issue would be for large data sets, and mostly you just have to hit break, cancel, or something of that nature if you try that.  Meanwhile, a search for permutation sort got me half a million hits.  The first hits were this pair of pages, but I also found stuff like: http://www.exso.com/courses/cs101b/slides/pure/node10.html  --[[User:Rdm|Rdm]] 13:13, 26 November 2010 (UTC)
