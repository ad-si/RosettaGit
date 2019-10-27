+++
title = "Talk:Hash from two arrays"
description = ""
date = 2017-02-14T17:32:48Z
aliases = []
[extra]
id = 1785
[taxonomies]
categories = []
tags = []
+++

== C# ==
 //I added this to just have some basic error checking
 int arg_length = arg_keys.Length == arg_values.Length ? arg_keys.Length : 0;

I'm not really sure such an absolutely not-understandable line should be on this repository, as most of the users won't even understand it. At the very least, there should be some sort of comment, or just change it and do it on a few lines instead of hashing the source code itself.
--[[User:CrashandDie|CrashandDie]] 09:42, 25 January 2007 (EST)

:I replaced the one-liner with an if statement. --[[User:Short Circuit|Short Circuit]] 10:02, 25 January 2007 (EST)

== Intent ==

What exactly is the intent behind this task?  It seems to specify the creation of a mapping of elements from one set to another, but what of the consequences of the hash requirement?  Is the mapping supposed to be lossy, in the sense that not all values will have a key in the map in the end?

I think the task needs to be clarified, perhaps with an additional or optional requirement to "prove" the validity of the code by providing input data and matching against an output state.  But I'm not sure what the relationship between input and output is supposed to be. --[[User:Short Circuit|Short Circuit]] 03:11, 8 May 2009 (UTC)

== My swiss cheese memory STRIKES AGAIN! ("library required" template?) ==

I seem to remember seeing a template or something that marks a code example as requiring a certain library. Am I remembering wrong? (If not, the first line of [[Creating a Hash from Two Arrays#Visual_Basic|the VB example]] needs updating.)
:<nowiki>{{libheader|library name}}</nowiki>. --[[User:Mwn3d|Mwn3d]] 20:46, 2 October 2009 (UTC)
::That would be it. Thanks. -- [[User:Eriksiers|Eriksiers]] 21:00, 2 October 2009 (UTC)
:::Unfortunately, the formatting of that template doesn't work well for CPAN modules, or anything else that uses colons for namespacing in the library name. --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 17:32, 14 February 2017 (UTC)

== Suggested move ==

I think that this task should be moved to a new "Associative arrays" sub-topic, something like "Associative arrays/From two arrays" or similar. Any reason not to? -- [[User:Eriksiers|Erik Siers]] 09:41, 12 March 2012 (UTC)
