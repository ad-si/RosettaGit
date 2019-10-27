+++
title = "Talk:Walk a directory/Non-recursively"
description = ""
date = 2010-02-06T15:13:17Z
aliases = []
[extra]
id = 1845
[taxonomies]
categories = []
tags = []
+++

Some of these examples recurse and some do not.  They should be different tasks.
:Done.  See [[Walk Directory Tree]] --[[User:Short Circuit|Short Circuit]] 11:00, 28 January 2007 (EST)

This task is unclear. When I first read it I thought it meant "print the <i>names</i> of the files that match a pattern. Only later did I realize that the <i>content</i> of the files was meant. [[User:Sgeier|Sgeier]] 00:59, 31 January 2007 (EST)
:Gah.  It should read names; File I/O is outside the scope of this task.  I'll fix the task description, but the programming examples will need review. --[[User:Short Circuit|Short Circuit]] 09:50, 31 January 2007 (EST)

:: I don't know Ada or Haskell, the others now do the right (clarified) thing. [[User:Sgeier|Sgeier]] 01:50, 1 February 2007 (EST)

== globs ==

Using a glob is not the same as walking a directory. A number of the solutions are pointless.
:Go ahead and update the task page with a more specific description; It was done once before with walking trees.  (Also, sign your posts with - - ~ ~ ~ ~ (sans spaces.) --[[User:Short Circuit|Short Circuit]] 19:58, 9 October 2007 (MDT)

== What does "match a pattern" mean? ==

For example, if the pattern is "bar", do you want it to match a file named "foobarbaz"? Some examples on here do and some don't. Also, what kind of patterns are allowed? Some people use Perl regular expressions, where ".*" is used to represent "0 or more characters of any kind"; but some use the shell globbing syntax, which use "*" to do that. So what do we want here? --[[User:Spoon!|Spoon!]] 05:37, 2 January 2009 (UTC)
:: I should think that "pattern" would just mean "whatever is normal for the given language and OS." For example, what I did in BASIC on DOS (<code>f = DIR$("*.*")</code>) is different from what I'd do in a shell script on Linux. -- [[User:Eriksiers|Eriksiers]] 16:00, 7 August 2009 (UTC)
