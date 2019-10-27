+++
title = "Category talk:Programming paradigm/Dynamic"
description = ""
date = 2013-01-23T15:24:15Z
aliases = []
[extra]
id = 12842
[taxonomies]
categories = []
tags = []
+++

I don't like the sentence "Dynamic programming languages are also commonly called scripting languages.". Although all scripting languages are dynamic languages, the reverse is not necessarily true. For example, Smalltalk and Lisp, both very dynamic languages is not generally considered to be a scripting languages. Although they both could be used as such, this is not their typical use.
Any opinion, anyone?

:True. Scripting languages are all dynamic but not vice-versa. (And scripting languages fill a very useful hole too). --[[User:Paddy3118|Paddy3118]] 13:18, 23 January 2013 (UTC)

:: I suppose the difference between a “static language” and a “dynamic language” is whether the binding of a token in the language (e.g., a function name) to an implementation of that token (the function itself, in a semantic sense) happens early or late. There's both benefits and costs to being late; the benefits are that you can substantially modify the behavior of the system at runtime, which can be a very neat trick indeed (and I do it a lot in my own code!), and the costs are that it tends to inhibit optimization and pushes the detection of many errors to run time. (Of course, you can't detect all problems at declaration/compilation time; that's a variation on the theme of the halting problem…) –[[User:Dkf|Donal Fellows]] 15:24, 23 January 2013 (UTC)
