+++
title = "Talk:Loop Structures"
description = ""
date = 2009-07-14T11:47:08Z
aliases = []
[extra]
id = 2124
[taxonomies]
categories = []
tags = []
+++

Python:

  with foo() as bar:
    baz(bar)

So where's the loop?

== Iteration ==

Examples from this page should be translated to pages in [[:Category:Iteration]]. We don't need duplicates. --[[User:Mwn3d|Mwn3d]] 12:46, 14 April 2008 (MDT)
:Hard to classify things sometimes... (but maybe it's just too late tonight:D) --[[User:ShinTakezou|ShinTakezou]] 00:54, 17 February 2009 (UTC)
::I looked at your edits and I classified some of them like this:
::*"unconditional loop": [[Loop/Infinite]]
::*"while loop": [[Loop/While]]
::*"for-like": [[Loop/For]]
::*"do-cycle": [[Loop/Continue]]
::Also, you could probably make "step" negative in the "for-like" loop to use it in the [[Loop/Downward For]] page. --[[User:Mwn3d|Mwn3d]] 19:32, 17 February 2009 (UTC)
:::Yes, but I start always too late in the night so I get confused quickly:) There are already Fortran example, even though misleading (e.g. Loop/Continue does not shows how to "continue", in the C sense, a loop in fortran...). I try to "fix" this, hopefully I won't make any mistake. --[[User:ShinTakezou|ShinTakezou]] 23:49, 17 February 2009 (UTC)

==Use of Loop/ pages?==

The task description says "'''Examples to be posted here should instead be posted in an appropriate Iteration page'''" -- but there are lots of examples. Should this be fixed? Perhaps this page should not be marked as a task? Or is the intent that this contains an overview, without detailed examples, of the language's looping constructs? --[[User:Kevin Reid|Kevin Reid]] 00:48, 20 May 2009 (UTC)
:It's definitely something that needs to be fixed. The idea was that the examples here were supposed to move to their proper Loop pages, but no one really noticed. If you can help out with it go ahead. I think content can simply be removed from here as it's migrated until we feel comfortable deleting the page. --[[User:Mwn3d|Mwn3d]] 01:06, 20 May 2009 (UTC)

== Loop n times ==

At least one loop structure does not yet have a separate task: the '''Repeat n''' loop, which is available at least on [[Logo]] and [[Vedit macro language]]. Example:

```vedit

Repeat(100) {
    Message("Spam")
}

```

--[[User:PauliKL|PauliKL]] 11:47, 14 July 2009 (UTC)
