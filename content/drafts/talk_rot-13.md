+++
title = "Talk:Rot-13"
description = ""
date = 2019-01-24T03:52:35Z
aliases = []
[extra]
id = 2373
[taxonomies]
categories = []
tags = []
+++

== utility program ==

:''Optionally wrap this function in a utility program which acts like a common Unix utility, performing a line-by-line rot-13 encoding of every line of input contained in each file listed on its command line, or (if no filenames are passed thereon) acting as a filter on its "standard input." (A number of UNIX scripting languages and utilities, such as awk and sed either default to processing files in this way or have command line switches or modules to easily implement these wrapper semantics, i.e. Perl and Python).''
This part seems excessive, and makes it more difficult to compare languages.  A separate task [[Command Line Arguments]], with attention paid to argument count, would be better for the command-line argument side, and [[User Input]] already handles reading from standard input. --[[User:Short Circuit|Short Circuit]] 19:36, 30 November 2007 (MST)

== Python ==

The implementations for version 2 and version 3 are almost identical.  It would be more illuminating to simply highlight the version 3 changes rather than the current cut-and-paste.
''(Preceding unsigned comment by [[User:Glennj]])''

I went ahead and made that change. Also, as implemented it was recomputing the translation table for each line, which doesn't make much sense. I pulled that out into a constant, and made the docstrings a little more concise. --[[User:NoLemurs|NoLemurs]] ([[User talk:NoLemurs|talk]]) 01:58, 9 May 2018 (UTC)

== SNOBOL4 ==
Replaced the original with function versions that handle both upper/lower case. --[[User:Snoman|Snoman]] 22:24, 10 July 2010 (UTC)
== C++/C++11 ==
Consider replacing or removing the C++ version.
