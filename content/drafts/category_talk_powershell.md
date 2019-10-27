+++
title = "Category talk:PowerShell"
description = ""
date = 2009-08-29T19:18:54Z
aliases = []
[extra]
id = 4792
[taxonomies]
categories = []
tags = []
+++

==Add-Type and the various tasks that call for implementing data structures or classes==
PowerShell v2 includes the Add-Type cmdlet. This accepts a string containing a class (or method) definition in C#, VB.NET or JScript and compiles it to a usable type. As far as PowerShell is concerned this is the only way to define own classes in the language.
The only downside is that they aren't really written in PowerShell that way, but rather in C# (as default at least). Would that even count as a PowerShell solution then? Chances are it'd be very similar to the C# solution, except wrapped in a string and followed by an <code>Add-Type $source</code>. —[[User:Hypftier|Johannes Rössel]] 19:18, 29 August 2009 (UTC)
