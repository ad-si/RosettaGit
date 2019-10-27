+++
title = "Talk:String Character Length"
description = ""
date = 2007-05-14T23:12:37Z
aliases = []
[extra]
id = 1885
[taxonomies]
categories = []
tags = []
+++

The term "string length" is ambiguous -- length can be in bytes or in characters, which will be different in a general unicodde situation. The TCL example is pretty much the only UTF-aware "string length". [[User:Sgeier|Sgeier]] 18:25, 1 February 2007 (EST)
:I created two copies of the page, one at [[String Character Length]], one at [[String Byte Length]].  Each has its own clarification of the "string length" task, but the programming examples require review to ensure that they still conform to the task description. --[[User:Short Circuit|Short Circuit]] 18:51, 1 February 2007 (EST)
::I'm glad to see this change. Many programs break horribly when they encounter a character outside the Basic Multilingual Plane. Most programmers are completely unaware about how horribly leaky the abstractions are. I'll try to add some information about this over the next few days. [[User:Hoqua|Hoqua]] 11:58, 2 February 2007 (EST)

I think that current description is still problematic.  In current
Unicode standard characters are pretty complicated beasts: a single
Unicode character may be composed of many Unicode code-points.
I am affraid that no programming language of today has "full"
Unicode support (there are third party libraries, also some word
processors claim full support).  So, I think that reasonable
compromise is to present what language offer (and explain 
_exactly_ what builtin support is computing).
